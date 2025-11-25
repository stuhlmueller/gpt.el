"""Integration tests for gpt.py with actual API calls.

These tests make real API calls and require valid API keys to be set in environment variables.
Unit tests for individual providers are in providers/test_*.py files.
"""

import io
import os
import sys

import pytest
from google.genai import errors as genai_errors

# Import functions and constants from gpt.py and providers
from gpt import print_and_collect
from providers.anthropic_provider import stream_anthropic
from providers.google_provider import stream_google
from providers.openai_provider import call_openai

# Common parameters for integration tests
MAX_TOKENS_INT = 200  # Reduced for faster tests
TEMPERATURE_THINKING_FLOAT = 1.0  # Must be 1 for Anthropic thinking
TEMPERATURE_NO_THINKING_FLOAT = 0.0

# Test prompts
SIMPLE_MATH_PROMPT = "User: What is 5 + 3? Please show your work step by step."
SIMPLE_GREETING_PROMPT = "User: Hello!"
WEB_SEARCH_PROMPT = "User: What were the top 3 news headlines on the BBC News website in the last hour?"


@pytest.mark.integration
@pytest.mark.anthropic
def test_anthropic_basic_integration(monkeypatch) -> None:
    """Integration test for basic Anthropic functionality with real API."""
    api_key = os.environ.get("ANTHROPIC_API_KEY")
    if not api_key or api_key == "NOT SET":
        pytest.skip("Anthropic API key not set or invalid, skipping integration test.")

    model = "claude-3-haiku-20240307"  # Using a fast, cheap model for tests

    mock_stdout = io.StringIO()
    monkeypatch.setattr(sys, "stdout", mock_stdout)

    try:
        stream = stream_anthropic(
            prompt=SIMPLE_GREETING_PROMPT,
            api_key=api_key,
            model=model,
            max_tokens=MAX_TOKENS_INT,
            temperature=TEMPERATURE_NO_THINKING_FLOAT,
            thinking_enabled=False,
        )
        print_and_collect(stream, api_type="anthropic")

        stdout_val = mock_stdout.getvalue()

        # Basic check that we got a response
        assert len(stdout_val) > 0
        assert any(greeting in stdout_val.lower() for greeting in ["hello", "hi", "hey", "greetings"])

    except Exception as e:
        pytest.fail(f"Anthropic integration test failed: {e}")


@pytest.mark.integration
@pytest.mark.anthropic
def test_anthropic_thinking_integration(monkeypatch) -> None:
    """Integration test for Anthropic with thinking mode enabled."""
    api_key = os.environ.get("ANTHROPIC_API_KEY")
    if not api_key or api_key == "NOT SET":
        pytest.skip("Anthropic API key not set or invalid, skipping integration test.")

    model = "claude-opus-4-0"  # Model that supports thinking
    budget_val = 1024

    mock_stdout = io.StringIO()
    monkeypatch.setattr(sys, "stdout", mock_stdout)

    try:
        stream = stream_anthropic(
            prompt=SIMPLE_MATH_PROMPT,
            api_key=api_key,
            model=model,
            max_tokens=budget_val + 100,
            temperature=TEMPERATURE_THINKING_FLOAT,
            thinking_enabled=True,
            thinking_budget=budget_val,
            interleaved_thinking=True,
        )
        print_and_collect(stream, api_type="anthropic")

        stdout_val = mock_stdout.getvalue()

        # Check we got the correct answer
        assert "8" in stdout_val or "eight" in stdout_val.lower()
        # Check thinking indicators appeared
        assert "[Thinking" in stdout_val

    except Exception as e:
        pytest.fail(f"Anthropic thinking integration test failed: {e}")


@pytest.mark.integration
@pytest.mark.openai
def test_openai_basic_integration(monkeypatch) -> None:
    """Integration test for basic OpenAI functionality with real API."""
    api_key = os.environ.get("OPENAI_API_KEY")
    if not api_key or api_key == "NOT SET":
        pytest.skip("OpenAI API key not set or invalid, skipping integration test.")

    model = "gpt-4o-mini"  # Using a cheaper model for tests

    mock_stdout = io.StringIO()
    monkeypatch.setattr(sys, "stdout", mock_stdout)

    try:
        stream = call_openai(
            prompt=SIMPLE_GREETING_PROMPT,
            api_key=api_key,
            model=model,
            max_tokens=MAX_TOKENS_INT,
            temperature=TEMPERATURE_NO_THINKING_FLOAT,
        )
        print_and_collect(stream, api_type="openai")

        stdout_val = mock_stdout.getvalue()

        # Basic check that we got a response
        assert len(stdout_val) > 0
        assert any(greeting in stdout_val.lower() for greeting in ["hello", "hi", "hey", "greetings"])

    except Exception as e:
        pytest.fail(f"OpenAI integration test failed: {e}")


@pytest.mark.integration
@pytest.mark.google
def test_google_basic_integration(monkeypatch) -> None:
    """Integration test for basic Google Gemini functionality with real API."""
    api_key = os.environ.get("GOOGLE_API_KEY")
    if not api_key or api_key == "NOT SET":
        pytest.skip("Google API key not set or invalid, skipping integration test.")

    # Use a generally available preview model; override with GOOGLE_MODEL if set.
    model = os.environ.get("GOOGLE_MODEL", "gemini-3-pro-preview")

    mock_stdout = io.StringIO()
    monkeypatch.setattr(sys, "stdout", mock_stdout)

    try:
        stream = stream_google(
            prompt=SIMPLE_GREETING_PROMPT,
            api_key=api_key,
            model=model,
            max_tokens=MAX_TOKENS_INT,
            temperature=TEMPERATURE_NO_THINKING_FLOAT,
        )
        print_and_collect(stream, api_type="google")

        stdout_val = mock_stdout.getvalue()

        # Basic check that we got a response
        assert len(stdout_val) > 0
        assert any(greeting in stdout_val.lower() for greeting in ["hello", "hi", "hey", "greetings"])

    except genai_errors.ClientError as e:
        if "NOT_FOUND" in str(e):
            pytest.skip(f"Google model {model} not available; skipping.")
        raise
    except Exception as e:
        pytest.fail(f"Google integration test failed: {e}")


@pytest.mark.integration
@pytest.mark.anthropic
def test_anthropic_thinking_difference_integration(monkeypatch) -> None:
    """Integration test ensuring thinking mode produces different output than non-thinking mode."""
    api_key = os.environ.get("ANTHROPIC_API_KEY")
    if not api_key or api_key == "NOT SET":
        pytest.skip("Anthropic API key not set or invalid, skipping integration test.")

    model = "claude-opus-4-0"  # Model that supports thinking
    budget_val = 1024
    prompt = SIMPLE_MATH_PROMPT

    def _run_call(*, enabled: bool) -> str:
        stdout_buf = io.StringIO()
        monkeypatch.setattr(sys, "stdout", stdout_buf)

        stream = stream_anthropic(
            prompt=prompt,
            api_key=api_key,
            model=model,
            max_tokens=budget_val + 100,
            temperature=(TEMPERATURE_THINKING_FLOAT if enabled else TEMPERATURE_NO_THINKING_FLOAT),
            thinking_enabled=enabled,
            thinking_budget=budget_val,
            interleaved_thinking=enabled,
        )
        print_and_collect(stream, api_type="anthropic")

        # Restore stdout
        monkeypatch.setattr(sys, "stdout", sys.__stdout__)
        return stdout_buf.getvalue()

    try:
        output_with_thinking = _run_call(enabled=True)
        output_without_thinking = _run_call(enabled=False)

        # Both should have the correct answer
        assert "8" in output_with_thinking or "eight" in output_with_thinking.lower()
        assert "8" in output_without_thinking or "eight" in output_without_thinking.lower()

        # Outputs should be different when thinking is enabled
        assert output_with_thinking != output_without_thinking, "Outputs should differ when thinking mode is toggled"

        # Thinking output should have thinking indicators
        assert "[Thinking" in output_with_thinking
        assert "[Thinking" not in output_without_thinking

    except Exception as e:
        pytest.fail(f"Anthropic thinking difference integration test failed: {e}")
