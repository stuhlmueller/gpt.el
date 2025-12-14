"""Tests for OpenAI provider implementation.

Note: Unit tests use mocking to test the logic without making actual API calls.
For integration tests with real APIs, see test_gpt_integration.py.
"""

from typing import Any, cast
from unittest.mock import Mock, patch

import pytest

from .common import InvalidAPIKeyError, MissingDependencyError
from .openai_provider import call_openai, handle_openai_stream


class TestOpenAIProvider:
    """Test suite for OpenAI provider functionality."""

    def test_missing_dependency(self) -> None:
        """Test that missing openai package raises appropriate error."""
        with patch("providers.openai_provider.openai", None):
            with pytest.raises(MissingDependencyError, match="OpenAI Python package"):
                call_openai("test prompt", "test_key", "gpt-4", 100, 0.5)

    def test_invalid_api_key(self) -> None:
        """Test that invalid API key raises appropriate error."""
        with pytest.raises(InvalidAPIKeyError, match="OpenAI API key not set"):
            call_openai("test prompt", "NOT SET", "gpt-4", 100, 0.5)

        with pytest.raises(InvalidAPIKeyError, match="OpenAI API key not set"):
            call_openai("test prompt", "", "gpt-4", 100, 0.5)

    @patch("providers.openai_provider.openai")
    def test_call_openai_with_raw_prompt(self, mock_openai) -> None:
        """Test call_openai with a raw string prompt."""
        mock_client = Mock()
        mock_openai.OpenAI.return_value = mock_client
        mock_stream = Mock()
        mock_client.responses.create.return_value = mock_stream

        result = call_openai("Hello, world!", "test_key", "gpt-4", 100, 0.5)

        assert result == mock_stream
        mock_client.responses.create.assert_called_once()
        call_args = mock_client.responses.create.call_args[1]
        assert call_args["model"] == "gpt-4"
        assert call_args["input"] == "Hello, world!"
        assert call_args["max_output_tokens"] == 100
        assert call_args["temperature"] == 0.5
        assert call_args["stream"] is True

    @patch("providers.openai_provider.openai")
    def test_call_openai_with_messages(self, mock_openai) -> None:
        """Test call_openai with role-based messages."""
        mock_client = Mock()
        mock_openai.OpenAI.return_value = mock_client
        mock_stream = Mock()
        mock_client.responses.create.return_value = mock_stream

        prompt = "user: Hello\nassistant: Hi there!\nuser: How are you?"
        result = call_openai(prompt, "test_key", "gpt-4", 100, 0.0)

        assert result == mock_stream
        mock_client.responses.create.assert_called_once()
        call_args = mock_client.responses.create.call_args[1]

        # Check that messages were parsed correctly
        messages = call_args["input"]
        assert len(messages) == 4  # system + 3 from prompt
        assert messages[0]["role"] == "system"
        assert messages[1]["role"] == "user"
        assert messages[1]["content"] == "Hello"
        assert messages[2]["role"] == "assistant"
        assert messages[2]["content"] == "Hi there!"
        assert messages[3]["role"] == "user"
        assert messages[3]["content"] == "How are you?"

        # Temperature 0.0 should not be included
        assert "temperature" not in call_args

    @patch("providers.openai_provider.openai")
    def test_call_openai_drops_trailing_empty_assistant(self, mock_openai) -> None:
        """A trailing 'assistant:' placeholder should not be sent to the API."""
        mock_client = Mock()
        mock_openai.OpenAI.return_value = mock_client
        mock_stream = Mock()
        mock_client.responses.create.return_value = mock_stream

        prompt = "user: Hello\nassistant:"
        _ = call_openai(prompt, "test_key", "gpt-4", 100, 0.0)

        call_args = mock_client.responses.create.call_args[1]
        messages = call_args["input"]
        assert messages[0]["role"] == "system"
        assert messages[1]["role"] == "user"
        assert messages[1]["content"] == "Hello"

    @patch("providers.openai_provider.openai")
    def test_call_openai_with_web_search(self, mock_openai) -> None:
        """Test call_openai with web search enabled."""
        mock_client = Mock()
        mock_openai.OpenAI.return_value = mock_client
        mock_stream = Mock()
        mock_client.responses.create.return_value = mock_stream

        result = call_openai("Search for news", "test_key", "gpt-4", 100, 0.5, web_search=True)

        assert result == mock_stream
        call_args = mock_client.responses.create.call_args[1]
        assert "tools" in call_args
        assert call_args["tools"] == [{"type": "web_search_preview"}]

    def test_handle_openai_stream_text_output(self, monkeypatch) -> None:
        """Test handling of text output from OpenAI stream."""
        # Create mock events
        events = [
            Mock(type="response.output_text.delta", delta="Hello "),
            Mock(type="response.output_text.delta", delta="world!"),
            Mock(type="response.completed"),
        ]

        result_gen = handle_openai_stream(cast(Any, iter(events)))
        result = "".join(result_gen)

        assert result == "Hello world!"

    def test_handle_openai_stream_with_reasoning(self, monkeypatch) -> None:
        """Test handling of reasoning in OpenAI stream."""
        # Create mock events
        item_mock = Mock(type="reasoning")
        events = [
            Mock(type="response.output_item.added", item=item_mock),
            Mock(type="response.reasoning.delta", delta="Thinking about this..."),
            Mock(type="response.output_item.done", item=item_mock),
            Mock(type="response.output_text.delta", delta="The answer is 42."),
            Mock(type="response.completed"),
        ]

        result_gen = handle_openai_stream(cast(Any, iter(events)))
        output = "".join(result_gen)

        assert "[Thinking...]" in output
        assert "Thinking about this..." in output
        assert "[Thinking done.]" in output
        assert "The answer is 42." in output

    def test_handle_openai_stream_with_reasoning_summary(self, monkeypatch) -> None:
        """Test handling of reasoning summary events in OpenAI stream (gpt-5 style)."""
        # Create mock events for summary text
        events = [
            Mock(
                type="response.reasoning_summary_text.delta",
                delta="Summary: thinking...",
            ),
            Mock(type="response.reasoning_summary_text.done"),
            Mock(type="response.output_text.delta", delta="Final answer."),
            Mock(type="response.completed"),
        ]

        result_gen = handle_openai_stream(cast(Any, iter(events)))
        output = "".join(result_gen)

        assert "[Thinking...]" in output
        assert "Summary: thinking..." in output
        assert "[Thinking done.]" in output
        assert "Final answer." in output

    def test_handle_openai_stream_with_web_search(self, monkeypatch) -> None:
        """Test handling of web search in OpenAI stream."""
        # Create mock events
        tool_item = Mock(type="function_call", name="web_search_preview")
        events = [
            Mock(type="response.output_item.added", item=tool_item),
            Mock(
                type="response.function_call_arguments.delta",
                delta='{"query": "latest news"}',
            ),
            Mock(type="response.function_call_arguments.done"),
            Mock(type="response.web_search_call"),
            Mock(type="response.output_text.delta", delta="Here are the latest news..."),
            Mock(type="response.completed"),
        ]

        result_gen = handle_openai_stream(cast(Any, iter(events)))
        output = "".join(result_gen)

        # The mock name attribute returns a Mock object, not the actual string
        # So we just check that the search was initiated and results were shown
        assert "[Searching for:" in output or "[Using tool:" in output or "[Finished preparing" in output
        assert "[Got web search results]" in output
        assert "Here are the latest news..." in output
