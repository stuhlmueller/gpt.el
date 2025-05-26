"""Common utilities and classes for provider implementations."""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import Any


@dataclass
class Message:
    """Represents a chat message with role and content."""

    role: str
    content: str


class APIError(Exception):
    """Base exception for API-related errors."""

    pass


class MissingDependencyError(APIError):
    """Raised when a required package is not installed."""

    pass


class InvalidAPIKeyError(APIError):
    """Raised when an API key is invalid or not set."""

    pass


def parse_messages(
    prompt: str, supported_roles: set[str] | None = None
) -> list[Message]:
    """Parse a prompt string into a list of Message objects.

    Args:
        prompt: The prompt string containing role-based messages
        supported_roles: Set of supported roles (case-insensitive). If None, defaults to common roles.

    Returns:
        List of Message objects parsed from the prompt
    """
    if supported_roles is None:
        supported_roles = {"user", "assistant", "human", "model"}

    role_pattern = "|".join(supported_roles)
    pattern = re.compile(
        rf"^({role_pattern}):(.+?)(?=\n(?:{role_pattern}):|\Z)", re.S | re.M | re.I
    )

    messages: list[Message] = []
    for match in pattern.finditer(prompt):
        role = match.group(1).lower()
        content = match.group(2).strip()
        messages.append(Message(role=role, content=content))

    return messages


def check_dependency(module: Any, name: str, install_cmd: str) -> None:
    """Check if a required dependency is available and raise error if not."""
    if module is None:
        raise MissingDependencyError(f"{name} not installed – {install_cmd}")


def validate_api_key(api_key: str, api_name: str) -> None:
    """Validate that API key is properly set."""
    if api_key == "NOT SET" or not api_key.strip():
        raise InvalidAPIKeyError(f"{api_name} API key not set or invalid.")


# Constants
DEFAULT_SYSTEM_MESSAGE = "You are a helpful assistant."
DEFAULT_THINKING_BUDGET = 10000
DEFAULT_GOOGLE_MAX_TOKENS = 32000
