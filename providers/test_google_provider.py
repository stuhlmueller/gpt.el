"""Tests for Google provider implementation.

Note: Unit tests use mocking to test the logic without making actual API calls.
For integration tests with real APIs, see test_gpt_integration.py.
"""

from unittest.mock import Mock, patch

import pytest

from .common import APIError, InvalidAPIKeyError, MissingDependencyError
from .google_provider import handle_google_stream, stream_google


class TestGoogleProvider:
    """Test suite for Google provider functionality."""

    def test_missing_dependency(self) -> None:
        """Test that missing genai package raises appropriate error."""
        with patch("providers.google_provider.genai", None):
            with pytest.raises(MissingDependencyError, match="Google GenAI package"):
                # Must consume generator to trigger the check
                list(stream_google("test prompt", "test_key", "gemini-pro", 100, 0.5))

    def test_invalid_api_key(self) -> None:
        """Test that invalid API key raises appropriate error."""
        with pytest.raises(InvalidAPIKeyError, match="Google API key not set"):
            # Must consume generator to trigger the check
            list(stream_google("test prompt", "NOT SET", "gemini-pro", 100, 0.5))

        with pytest.raises(InvalidAPIKeyError, match="Google API key not set"):
            list(stream_google("test prompt", "", "gemini-pro", 100, 0.5))

    @patch("providers.google_provider.genai")
    @patch("providers.google_provider.genai_types")
    def test_stream_google_with_raw_prompt(self, mock_genai_types, mock_genai) -> None:
        """Test stream_google with a raw string prompt."""
        mock_client = Mock()
        mock_genai.Client.return_value = mock_client
        mock_chunk = Mock(text="Hello")
        mock_client.models.generate_content_stream.return_value = [mock_chunk]

        # Mock the types
        mock_genai_types.UserContent = Mock
        mock_genai_types.Part.from_text = Mock(return_value=Mock())
        mock_genai_types.GenerateContentConfig = Mock

        # Consume the generator to trigger the API call
        result = list(stream_google("Hello, world!", "test_key", "gemini-pro", 100, 0.5))

        assert result == [mock_chunk]
        mock_client.models.generate_content_stream.assert_called_once()
        call_args = mock_client.models.generate_content_stream.call_args[1]

        assert call_args["model"] == "gemini-pro"
        assert len(call_args["contents"]) == 1

    @patch("providers.google_provider.genai")
    @patch("providers.google_provider.genai_types")
    def test_stream_google_with_messages(self, mock_genai_types, mock_genai) -> None:
        """Test stream_google with role-based messages."""
        mock_client = Mock()
        mock_genai.Client.return_value = mock_client
        mock_chunk = Mock(text="Response")
        mock_client.models.generate_content_stream.return_value = [mock_chunk]

        # Mock the types
        mock_genai_types.UserContent = Mock
        mock_genai_types.ModelContent = Mock
        mock_genai_types.Part.from_text = Mock(return_value=Mock())
        mock_genai_types.GenerateContentConfig = Mock

        prompt = "user: Hello\nmodel: Hi there!\nhuman: How are you?"
        # Consume the generator to trigger the API call
        result = list(stream_google(prompt, "test_key", "gemini-pro", 100, 0.0))

        assert result == [mock_chunk]
        call_args = mock_client.models.generate_content_stream.call_args[1]

        # Check that messages were parsed correctly
        contents = call_args["contents"]
        assert len(contents) == 3

    @patch("providers.google_provider.genai")
    @patch("providers.google_provider.genai_types")
    def test_stream_google_max_tokens_limit(self, mock_genai_types, mock_genai) -> None:
        """Test that max_tokens is capped at DEFAULT_GOOGLE_MAX_TOKENS."""
        mock_client = Mock()
        mock_genai.Client.return_value = mock_client
        mock_chunk = Mock(text="Response")
        mock_client.models.generate_content_stream.return_value = [mock_chunk]

        # Mock the types
        mock_genai_types.UserContent = Mock
        mock_genai_types.Part.from_text = Mock(return_value=Mock())
        mock_config = Mock()
        mock_genai_types.GenerateContentConfig = mock_config

        # Request more than the limit - consume generator to trigger the call
        list(stream_google("Hello", "test_key", "gemini-pro", 50000, 0.5))

        # Check that config was created with the capped value
        # Get the call args from the mock
        assert mock_config.called
        # Access the kwargs passed to GenerateContentConfig
        call_kwargs = mock_config.call_args.kwargs
        assert call_kwargs["max_output_tokens"] == 32000  # DEFAULT_GOOGLE_MAX_TOKENS

    @patch("providers.google_provider.genai")
    def test_stream_google_api_errors(self, mock_genai) -> None:
        """Test that API errors are properly wrapped."""
        mock_client = Mock()
        mock_genai.Client.return_value = mock_client

        # Test authentication error - must consume generator to trigger the error
        mock_client.models.generate_content_stream.side_effect = ValueError("Invalid api_key")
        with pytest.raises(InvalidAPIKeyError, match="Google API authentication failed"):
            list(stream_google("Hello", "test_key", "gemini-pro", 100, 0.5))

        # Test connection error
        mock_client.models.generate_content_stream.side_effect = ConnectionError("Network error")
        with pytest.raises(APIError, match="Google API connection error"):
            list(stream_google("Hello", "test_key", "gemini-pro", 100, 0.5))

        # Test timeout error
        mock_client.models.generate_content_stream.side_effect = TimeoutError("Request timeout")
        with pytest.raises(APIError, match="Google API timeout"):
            list(stream_google("Hello", "test_key", "gemini-pro", 100, 0.5))

    def test_handle_google_stream(self, monkeypatch) -> None:
        """Test handling of Google stream responses."""
        # Create mock chunks
        chunks = [
            Mock(text="Hello "),
            Mock(text="from "),
            Mock(text="Gemini!"),
            Mock(text=None),  # Should be ignored
        ]

        result_gen = handle_google_stream(iter(chunks))
        result = "".join(result_gen)

        assert result == "Hello from Gemini!"
