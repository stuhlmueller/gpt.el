"""Tests for common utilities and classes."""

import pytest

from .common import (
    APIError,
    InvalidAPIKeyError,
    Message,
    MissingDependencyError,
    check_dependency,
    parse_messages,
    validate_api_key,
)


class TestMessage:
    """Test the Message dataclass."""

    def test_message_creation(self) -> None:
        """Test creating a Message instance."""
        msg = Message(role="user", content="Hello")
        assert msg.role == "user"
        assert msg.content == "Hello"


class TestExceptions:
    """Test custom exception classes."""

    def test_api_error(self) -> None:
        """Test APIError exception."""
        with pytest.raises(APIError, match="Test error"):
            raise APIError("Test error")

    def test_missing_dependency_error(self) -> None:
        """Test MissingDependencyError exception."""
        with pytest.raises(MissingDependencyError, match="Package not found"):
            raise MissingDependencyError("Package not found")

        # Check it's a subclass of APIError
        assert issubclass(MissingDependencyError, APIError)

    def test_invalid_api_key_error(self) -> None:
        """Test InvalidAPIKeyError exception."""
        with pytest.raises(InvalidAPIKeyError, match="Invalid key"):
            raise InvalidAPIKeyError("Invalid key")

        # Check it's a subclass of APIError
        assert issubclass(InvalidAPIKeyError, APIError)


class TestParseMessages:
    """Test the parse_messages function."""

    def test_parse_simple_messages(self) -> None:
        """Test parsing simple user/assistant messages."""
        prompt = "user: Hello\nassistant: Hi there!"
        messages = parse_messages(prompt)

        assert len(messages) == 2
        assert messages[0].role == "user"
        assert messages[0].content == "Hello"
        assert messages[1].role == "assistant"
        assert messages[1].content == "Hi there!"

    def test_parse_multiline_messages(self) -> None:
        """Test parsing messages with multiple lines."""
        prompt = """user: Hello
How are you today?
assistant: I'm doing well!
Thanks for asking."""
        messages = parse_messages(prompt)

        assert len(messages) == 2
        assert messages[0].role == "user"
        assert messages[0].content == "Hello\nHow are you today?"
        assert messages[1].role == "assistant"
        assert messages[1].content == "I'm doing well!\nThanks for asking."

    def test_parse_custom_roles(self) -> None:
        """Test parsing with custom supported roles."""
        prompt = "human: Hello\nmodel: Hi!"
        messages = parse_messages(prompt, {"human", "model"})

        assert len(messages) == 2
        assert messages[0].role == "human"
        assert messages[0].content == "Hello"
        assert messages[1].role == "model"
        assert messages[1].content == "Hi!"

    def test_parse_case_insensitive(self) -> None:
        """Test that role parsing is case-insensitive."""
        prompt = "USER: Hello\nASSISTANT: Hi!"
        messages = parse_messages(prompt)

        assert len(messages) == 2
        assert messages[0].role == "user"
        assert messages[0].content == "Hello"
        assert messages[1].role == "assistant"
        assert messages[1].content == "Hi!"

    def test_parse_empty_prompt(self) -> None:
        """Test parsing an empty prompt."""
        messages = parse_messages("")
        assert len(messages) == 0

    def test_parse_no_roles(self) -> None:
        """Test parsing a prompt with no role markers."""
        messages = parse_messages("Just some text without roles")
        assert len(messages) == 0


class TestCheckDependency:
    """Test the check_dependency function."""

    def test_check_dependency_present(self) -> None:
        """Test when dependency is present (not None)."""
        # Should not raise any exception
        check_dependency("some_module", "Test Module", "pip install test")

    def test_check_dependency_missing(self) -> None:
        """Test when dependency is missing (None)."""
        with pytest.raises(MissingDependencyError, match="Test Module not installed"):
            check_dependency(None, "Test Module", "pip install test")


class TestValidateApiKey:
    """Test the validate_api_key function."""

    def test_validate_valid_key(self) -> None:
        """Test validation with a valid API key."""
        # Should not raise any exception
        validate_api_key("valid-api-key-123", "TestAPI")

    def test_validate_not_set_key(self) -> None:
        """Test validation with 'NOT SET' key."""
        with pytest.raises(InvalidAPIKeyError, match="TestAPI API key not set"):
            validate_api_key("NOT SET", "TestAPI")

    def test_validate_empty_key(self) -> None:
        """Test validation with empty key."""
        with pytest.raises(InvalidAPIKeyError, match="TestAPI API key not set"):
            validate_api_key("", "TestAPI")

    def test_validate_whitespace_key(self) -> None:
        """Test validation with whitespace-only key."""
        with pytest.raises(InvalidAPIKeyError, match="TestAPI API key not set"):
            validate_api_key("   ", "TestAPI")
