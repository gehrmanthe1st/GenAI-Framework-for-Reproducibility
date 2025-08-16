import os
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()

# Define configuration variables
OPENAI_API_KEY = os.getenv("OPENAI_API_KEY")
ANTHROPIC_API_KEY = os.getenv("ANTHROPIC_API_KEY")

if OPENAI_API_KEY is None or ANTHROPIC_API_KEY is None:
    raise ValueError("Missing one or more API keys in .env")
