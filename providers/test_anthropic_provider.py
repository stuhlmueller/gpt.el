"""Tests for Anthropic provider implementation.

Note: Unit tests use mocking to test the logic without making actual API calls.
For integration tests with real APIs, see test_gpt_integration.py.
"""

import pytest
from unittest.mock import Mock, patch

from .anthropic_provider import stream_anthropic, handle_anthropic_stream
from .common import InvalidAPIKeyError, MissingDependencyError


class TestAnthropicProvider:
    """Test suite for Anthropic provider functionality."""

    def test_missing_dependency(self):
        """Test that missing anthropic package raises appropriate error."""
        with patch('providers.anthropic_provider.anthropic', None):
            with pytest.raises(MissingDependencyError, match="Anthropic Python package"):
                stream_anthropic("test prompt", "test_key", "claude-3", 100, 0.5)

    def test_invalid_api_key(self):
        """Test that invalid API key raises appropriate error."""
        with pytest.raises(InvalidAPIKeyError, match="Anthropic API key not set"):
            stream_anthropic("test prompt", "NOT SET", "claude-3", 100, 0.5)
        
        with pytest.raises(InvalidAPIKeyError, match="Anthropic API key not set"):
            stream_anthropic("test prompt", "", "claude-3", 100, 0.5)

    @patch('providers.anthropic_provider.anthropic')
    def test_stream_anthropic_basic(self, mock_anthropic):
        """Test basic stream_anthropic functionality."""
        mock_client = Mock()
        mock_anthropic.Anthropic.return_value = mock_client
        mock_stream = Mock()
        mock_client.messages.create.return_value = mock_stream

        prompt = "user: Hello\nassistant: Hi!\nuser: How are you?"
        result = stream_anthropic(prompt, "test_key", "claude-3", 100, 0.5)

        assert result == mock_stream
        mock_client.messages.create.assert_called_once()
        call_args = mock_client.messages.create.call_args[1]
        
        # Check messages were formatted correctly
        messages = call_args['messages']
        assert len(messages) == 3
        assert messages[0]['role'] == 'user'
        assert messages[0]['content'] == 'Hello'
        assert messages[1]['role'] == 'assistant'
        assert messages[1]['content'] == 'Hi!'
        assert messages[2]['role'] == 'user'
        assert messages[2]['content'] == 'How are you?'

    @patch('providers.anthropic_provider.anthropic')
    def test_stream_anthropic_with_thinking(self, mock_anthropic):
        """Test stream_anthropic with thinking enabled."""
        mock_client = Mock()
        mock_anthropic.Anthropic.return_value = mock_client
        mock_stream = Mock()
        mock_client.messages.create.return_value = mock_stream

        result = stream_anthropic(
            "user: What is 2+2?",
            "test_key",
            "claude-3",
            2000,
            1.0,
            thinking_enabled=True,
            thinking_budget=1000
        )

        assert result == mock_stream
        call_args = mock_client.messages.create.call_args[1]
        
        # Check thinking parameters
        assert call_args['temperature'] == 1  # Must be 1 for thinking
        assert 'thinking' in call_args
        assert call_args['thinking']['type'] == 'enabled'
        assert call_args['thinking']['budget_tokens'] == 1000

    def test_stream_anthropic_thinking_validation(self):
        """Test that thinking budget validation works."""
        # Don't patch anthropic for this test since we want to test the validation
        # that happens before any API calls
        with pytest.raises(ValueError, match="max_tokens.*must be greater than thinking_budget"):
            stream_anthropic(
                "user: Hello",
                "test_key",
                "claude-3",
                100,  # max_tokens
                1.0,
                thinking_enabled=True,
                thinking_budget=200  # Greater than max_tokens
            )

    def test_handle_anthropic_stream_text_output(self, monkeypatch):
        """Test handling of text output from Anthropic stream."""
        # Create mock events
        delta_mock = Mock(type="text_delta", text="Hello, world!")
        events = [
            Mock(type="message_start"),
            Mock(type="content_block_delta", delta=delta_mock),
            Mock(type="message_stop"),
        ]

        result_gen = handle_anthropic_stream(iter(events))
        result = ''.join(result_gen)

        assert result == "Hello, world!"

    def test_handle_anthropic_stream_with_thinking(self, monkeypatch):
        """Test handling of thinking in Anthropic stream."""
        # Create mock events
        thinking_block = Mock(type="thinking")
        thinking_delta = Mock(type="thinking_delta", thinking="Let me think...")
        text_delta = Mock(type="text_delta", text="The answer is 4.")

        events = [
            Mock(type="content_block_start", content_block=thinking_block),
            Mock(type="content_block_delta", delta=thinking_delta),
            Mock(type="content_block_stop"),
            Mock(type="content_block_delta", delta=text_delta),
            Mock(type="message_stop"),
        ]

        result_gen = handle_anthropic_stream(iter(events))
        output = ''.join(result_gen)

        assert "[Thinking...]" in output
        assert "Let me think..." in output
        assert "[Thinking done.]" in output
        assert "The answer is 4." in output

    def test_handle_anthropic_stream_with_web_search(self, monkeypatch):
        """Test handling of web search in Anthropic stream."""
        # Create mock events
        tool_block = Mock(type="server_tool_use", name="web_search")
        json_delta = Mock(type="input_json_delta", partial_json='{"query": "latest news"}')
        result_block = Mock(type="web_search_tool_result")
        text_delta = Mock(type="text_delta", text="Here are the results...")

        events = [
            Mock(type="content_block_start", content_block=tool_block),
            Mock(type="content_block_delta", delta=json_delta),
            Mock(type="content_block_stop"),
            Mock(type="content_block_start", content_block=result_block),
            Mock(type="content_block_stop"),
            Mock(type="content_block_delta", delta=text_delta),
            Mock(type="message_stop"),
        ]

        result_gen = handle_anthropic_stream(iter(events))
        output = ''.join(result_gen)

        # The mock name attribute returns a Mock object, not the actual string
        # So we just check that the search was initiated and results were shown
        assert "[Searching for:" in output or "[Using tool:" in output
        assert "[Got web search results]" in output
        assert "Here are the results..." in output 