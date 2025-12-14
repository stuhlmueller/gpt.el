"""Google Gemini provider implementation for gpt.py."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Iterator, Optional

from .common import (
    DEFAULT_GOOGLE_MAX_TOKENS,
    APIError,
    InvalidAPIKeyError,
    check_dependency,
    drop_trailing_empty_messages,
    parse_messages,
    validate_api_key,
)

# Third-party imports (optional)
genai: Optional[Any] = None
genai_types: Optional[Any] = None
genai_errors: Optional[Any] = None

try:
    import google.genai as genai
    from google.genai import errors as genai_errors
    from google.genai import types as genai_types
except ImportError:
    pass

# Type checking imports
if TYPE_CHECKING:  # pragma: no cover
    from google.genai.types import GenerateContentResponse


def stream_google(
    prompt: str,
    api_key: str,
    model: str,
    max_tokens: int,
    temperature: float,
) -> Iterator["GenerateContentResponse"]:
    """Generate completions using Google's Gemini API."""

    check_dependency(genai, "Google GenAI package", "`pip install google‑genai`")
    validate_api_key(api_key, "Google")

    assert genai is not None
    assert genai_types is not None

    client = genai.Client(api_key=api_key)
    parsed_messages = parse_messages(prompt, {"user", "human", "assistant", "model"})
    parsed_messages = drop_trailing_empty_messages(parsed_messages, {"assistant", "model"})
    contents: list[Any]
    if parsed_messages:
        contents = []
        for msg in parsed_messages:
            factory = genai_types.UserContent if msg.role in {"user", "human"} else genai_types.ModelContent
            contents.append(factory(parts=[genai_types.Part.from_text(text=msg.content)]))
    else:
        contents = [genai_types.UserContent(parts=[genai_types.Part.from_text(text=prompt)])]

    generation_config_args: dict[str, Any] = {
        "max_output_tokens": min(DEFAULT_GOOGLE_MAX_TOKENS, max_tokens),
        "temperature": temperature,
    }

    try:
        config_params = {**generation_config_args}

        api_response = client.models.generate_content_stream(
            model=model,
            contents=contents,
            config=genai_types.GenerateContentConfig(**config_params),
        )
        # Yield from the stream to keep the client alive until consumption is complete
        yield from api_response
    except ValueError as e:
        if "api_key" in str(e).lower() or "authentication" in str(e).lower():
            raise InvalidAPIKeyError(f"Google API authentication failed: {e}") from e
        raise APIError(f"Google API parameter error: {e}") from e
    except ConnectionError as e:
        raise APIError(f"Google API connection error: {e}") from e
    except TimeoutError as e:
        raise APIError(f"Google API timeout: {e}") from e
    except Exception as e:
        # Handle Google-specific errors if the module is available
        if genai_errors is not None:
            if isinstance(e, genai_errors.ClientError):
                error_msg = str(e)
                if "NOT_FOUND" in error_msg:
                    raise APIError(f"Google API model not found: {e}") from e
                if "PERMISSION_DENIED" in error_msg or "authentication" in error_msg.lower():
                    raise InvalidAPIKeyError(f"Google API authentication failed: {e}") from e
                raise APIError(f"Google API client error: {e}") from e
            if isinstance(e, genai_errors.APIError):
                raise APIError(f"Google API error: {e}") from e
        raise


def handle_google_stream(stream: Iterator["GenerateContentResponse"]) -> Iterator[str]:
    """Handle Google streaming responses and yield text chunks."""
    for item in stream:
        chunk: Any = item
        text_val = getattr(chunk, "text", None)
        if text_val is not None:
            text = str(text_val)
            yield text
