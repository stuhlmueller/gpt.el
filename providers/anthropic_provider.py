"""Anthropic provider implementation for gpt.py."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Iterator, Optional

from .common import (
    DEFAULT_THINKING_BUDGET,
    APIError,
    InvalidAPIKeyError,
    check_dependency,
    parse_messages,
    validate_api_key,
)

# Third-party imports (optional)
anthropic: Optional[Any] = None

try:
    import anthropic
except ImportError:
    pass

# Type checking imports
if TYPE_CHECKING:  # pragma: no cover
    from anthropic.types import (
        MessageStreamEvent as AnthropicMessageStreamEvent,
    )


def stream_anthropic(
    prompt: str,
    api_key: str,
    model: str,
    max_tokens: int,
    temperature: float,
    thinking_enabled: bool = False,
    thinking_budget: int = DEFAULT_THINKING_BUDGET,
    interleaved_thinking: bool = False,
    web_search: bool = False,
) -> Iterator["AnthropicMessageStreamEvent"]:
    """Generate completions using Anthropic's API with optional extended thinking.

    Args:
        prompt: The input prompt containing user/assistant messages
        api_key: Anthropic API key
        model: Model name (e.g., 'claude-3-opus-20240229')
        max_tokens: Maximum tokens to generate
        temperature: Sampling temperature (must be 1.0 if thinking is enabled)
        thinking_enabled: Whether to enable extended thinking mode
        thinking_budget: Token budget for thinking (must be less than max_tokens)
        interleaved_thinking: Whether to enable interleaved thinking with tools
        web_search: Whether to enable web search tool

    Returns:
        Iterator of Anthropic message stream events

    Raises:
        MissingDependencyError: If anthropic package is not installed
        InvalidAPIKeyError: If API key is invalid
        ValueError: If thinking parameters are invalid
        APIError: For other API-related errors
    """

    check_dependency(anthropic, "Anthropic Python package", "`pip install anthropic")
    validate_api_key(api_key, "Anthropic")

    assert anthropic is not None

    client = anthropic.Anthropic(api_key=api_key)

    parsed_messages = parse_messages(prompt, {"user", "assistant"})
    messages: list[dict[str, str]] = []

    current_user_content: Optional[str] = None

    for msg in parsed_messages:
        if msg.role == "user":
            if current_user_content:
                current_user_content += f"\n\n{msg.content}"
            else:
                current_user_content = msg.content
        else:
            if current_user_content:
                messages.append({"role": "user", "content": current_user_content})
                current_user_content = None
            messages.append({"role": "assistant", "content": msg.content})

    if current_user_content:
        messages.append({"role": "user", "content": current_user_content})

    try:
        if thinking_enabled and max_tokens <= thinking_budget:
            raise ValueError(
                f"max_tokens ({max_tokens}) must be greater than thinking_budget ({thinking_budget}) "
                "when thinking is enabled. Please increase max_tokens or decrease thinking_budget."
            )

        kwargs: dict[str, Any] = {
            "model": model,
            "messages": messages,
            "max_tokens": max_tokens,
            "temperature": (1 if thinking_enabled else temperature),
            "stream": True,
        }

        if thinking_enabled:
            kwargs["thinking"] = {"type": "enabled", "budget_tokens": thinking_budget}

        headers: dict[str, str] = {}

        # Enable large context window for Sonnet 4.5
        if model == "claude-sonnet-4-5":
            headers["anthropic-beta"] = "context-1m-2025-08-07"

        # Note: interleaved-thinking and context-1m betas are mutually exclusive
        # If both are needed, only the first one set will be used
        if interleaved_thinking and "anthropic-beta" not in headers:
            headers["anthropic-beta"] = "interleaved-thinking-2025-05-14"

        if headers:
            kwargs["extra_headers"] = headers

        if web_search:
            kwargs["tools"] = [
                {
                    "name": "web_search",
                    "type": "web_search_20250305",
                }
            ]

        return client.messages.create(**kwargs)
    except anthropic.AuthenticationError as e:
        raise InvalidAPIKeyError(f"Anthropic authentication failed: {e}") from e
    except anthropic.RateLimitError as e:
        raise APIError(f"Anthropic rate limit exceeded: {e}") from e
    except anthropic.APIConnectionError as e:
        raise APIError(f"Anthropic connection error: {e}") from e
    except anthropic.APIError as e:
        raise APIError(f"Anthropic API error: {e}") from e


def handle_anthropic_stream(
    stream: Iterator["AnthropicMessageStreamEvent"],
) -> Iterator[str]:
    """Handle Anthropic streaming responses and yield text chunks."""
    current_block_type: Optional[str] = None
    current_tool_name: Optional[str] = None
    current_tool_input_buffer: str = ""

    for chunk in stream:
        chunk_type = getattr(chunk, "type", None)

        if chunk_type == "message_start":
            pass

        elif chunk_type == "content_block_start":
            content_block = getattr(chunk, "content_block", None)
            if content_block:
                current_block_type = getattr(content_block, "type", None)

                if current_block_type == "thinking":
                    yield "\n[Thinking...]\n"
                elif current_block_type == "server_tool_use":
                    tool_use_block = content_block
                    current_tool_name = getattr(tool_use_block, "name", None)
                    if current_tool_name == "web_search":
                        pass
                    elif current_tool_name:
                        yield f"\n[Using tool: {current_tool_name}...]"
                    current_tool_input_buffer = ""
                elif current_block_type == "web_search_tool_result":
                    yield "[Got web search results]"

        elif chunk_type == "content_block_delta":
            delta = getattr(chunk, "delta", None)
            if delta:
                delta_type = getattr(delta, "type", None)

                if delta_type == "text_delta":
                    text = getattr(delta, "text", None)
                    if text:
                        yield str(text)
                elif delta_type == "thinking_delta":
                    thinking_delta_obj = getattr(delta, "thinking", None)
                    if thinking_delta_obj:
                        text_content = str(thinking_delta_obj)
                        if text_content is not None:
                            yield str(text_content)
                elif delta_type == "input_json_delta":
                    if current_block_type == "server_tool_use":
                        partial_json = getattr(delta, "partial_json", None)
                        if partial_json:
                            current_tool_input_buffer += partial_json

        elif chunk_type == "content_block_stop":
            if current_block_type == "thinking":
                yield "\n[Thinking done.]\n"
            elif current_block_type == "server_tool_use":
                if current_tool_name == "web_search" and current_tool_input_buffer:
                    try:
                        import json

                        query_data = json.loads(current_tool_input_buffer)
                        search_query = query_data.get("query", "N/A")
                        yield f'\n[Searching for: "{search_query}"]'
                    except json.JSONDecodeError:
                        yield f"[JSON decode error for search query: {current_tool_input_buffer[:60]}...]"
                elif current_tool_name:
                    yield f"[Finished using tool: {current_tool_name}]\n"

                current_tool_name = None
                current_tool_input_buffer = ""
            elif current_block_type == "web_search_tool_result":
                pass

            current_block_type = None

        elif chunk_type == "message_delta":
            pass

        elif chunk_type == "message_stop":
            pass
