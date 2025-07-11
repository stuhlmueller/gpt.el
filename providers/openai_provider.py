"""OpenAI provider implementation for gpt.py."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Iterator, Optional

from .common import (
    DEFAULT_SYSTEM_MESSAGE,
    APIError,
    InvalidAPIKeyError,
    check_dependency,
    parse_messages,
    validate_api_key,
)

# Third-party imports (optional)
openai: Optional[Any] = None

try:
    import openai  # type: ignore
except ImportError:
    pass

# Type checking imports
if TYPE_CHECKING:  # pragma: no cover
    from openai import Stream as OpenAIStream

# Models that definitely don't support web search
WEB_SEARCH_UNSUPPORTED_MODELS = {
    "o3-pro",
    "o3-mini",
    "o4",
    "o4-mini",
}


def call_openai(
    prompt: str,
    api_key: str,
    model: str,
    max_tokens: int,
    temperature: float,
    web_search: bool = False,
) -> OpenAIStream[Any]:
    """Generate completions using OpenAI's Responses API.

    Args:
        prompt: The input prompt containing user/assistant messages or a raw string.
        api_key: OpenAI API key
        model: Model name (e.g., 'gpt-4', 'o4-mini', 'gpt-4.1')
        max_tokens: Maximum tokens to generate
        temperature: Sampling temperature (0.0-2.0)
        web_search: Whether to enable web search

    Returns:
        Stream of Responses API events

    Raises:
        MissingDependencyError: If openai package is not installed
        InvalidAPIKeyError: If API key is invalid
        APIError: For other API-related errors
    """
    check_dependency(openai, "OpenAI Python package", "`pip install -U openai`")
    validate_api_key(api_key, "OpenAI")

    assert openai is not None

    client = openai.OpenAI(api_key=api_key)

    try:
        parsed_messages = parse_messages(prompt, {"user", "assistant", "system"})

        api_input: list[dict[str, str]] | str
        if not parsed_messages:
            api_input = prompt
        else:
            final_input_messages: list[dict[str, str]] = []
            has_system_message = any(msg.role == "system" for msg in parsed_messages)

            if not has_system_message:
                final_input_messages.append({"role": "system", "content": DEFAULT_SYSTEM_MESSAGE})

            for msg in parsed_messages:
                final_input_messages.append({"role": msg.role, "content": msg.content})
            api_input = final_input_messages

        params: dict[str, Any] = {
            "model": model,
            "input": api_input,
            "stream": True,
            "max_output_tokens": max_tokens,
        }

        if web_search:
            # Check if the model explicitly doesn't support web search
            if model not in WEB_SEARCH_UNSUPPORTED_MODELS:
                params["tools"] = [{"type": "web_search_preview"}]

        if temperature != 0.0:
            params["temperature"] = temperature

        return client.responses.create(**params)

    except openai.AuthenticationError as e:
        raise InvalidAPIKeyError(f"OpenAI authentication failed: {e}") from e
    except openai.RateLimitError as e:
        raise APIError(f"OpenAI rate limit exceeded: {e}") from e
    except openai.APIConnectionError as e:
        raise APIError(f"OpenAI connection error: {e}") from e
    except openai.APIError as e:
        raise APIError(f"OpenAI API error: {e}") from e


def handle_openai_stream(stream: OpenAIStream[Any]) -> Iterator[str]:
    """Handle OpenAI Responses API streaming and yield text chunks."""
    current_tool_name: Optional[str] = None
    current_tool_args_buffer: str = ""
    in_reasoning = False

    import collections.abc as _abc

    if not isinstance(stream, _abc.Iterator):
        yield f"Warning: Received non-iterator OpenAI response: {stream}"
        return

    for event in stream:
        event_type = getattr(event, "type", None)

        if event_type == "response.output_text.delta":
            delta_text = getattr(event, "delta", "")
            if delta_text:
                yield delta_text

        elif event_type == "response.output_item.added":
            item = getattr(event, "item", None)
            if item:
                item_type = getattr(item, "type", None)
                if item_type == "reasoning":
                    yield "\n[Thinking...]\n"
                    in_reasoning = True
                elif item_type == "function_call":
                    current_tool_name = getattr(item, "name", None)
                    if current_tool_name and current_tool_name != "web_search_preview":
                        yield f"\n[Using tool: {current_tool_name}...]"
                    current_tool_args_buffer = ""

        elif event_type == "response.output_item.done":
            item = getattr(event, "item", None)
            if item:
                item_type = getattr(item, "type", None)
                if item_type == "reasoning" and in_reasoning:
                    yield "\n[Thinking done.]\n"
                    in_reasoning = False

        elif event_type == "response.function_call_arguments.delta":
            delta_args = getattr(event, "delta", "")
            if delta_args:
                current_tool_args_buffer += delta_args

        elif event_type == "response.function_call_arguments.done":
            if current_tool_name == "web_search_preview" and current_tool_args_buffer:
                try:
                    import json
                    args_data = json.loads(current_tool_args_buffer)
                    search_query = args_data.get("query", "N/A")
                    yield f"\n[Searching for: \"{search_query}\"]"
                except json.JSONDecodeError:
                    yield f"[JSON decode error for tool args: {current_tool_args_buffer[:60]}...]"
            elif current_tool_name:
                yield f"[Finished preparing {current_tool_name} call]"

        elif event_type == "response.reasoning.delta":
            reasoning_text = getattr(event, "delta", "")
            if reasoning_text:
                yield reasoning_text

        elif event_type == "response.web_search_call":
            yield "[Got web search results]"

        elif event_type == "response.error":
            error_details = getattr(event, "error", None)
            yield f"\nError in Responses API stream: {error_details}"
            break

        elif event_type == "response.completed":
            pass

        elif event_type in ["response.created", "response.in_progress",
                            "response.content_part.added",
                            "response.output_text.done", "response.content_part.done"]:
            pass
