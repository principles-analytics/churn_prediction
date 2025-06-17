my_function <- function() {
      chat <- ellmer::chat_ollama(
        model = "deepseek-r1",
        system_prompt = "You are a friendly but terse assistant.",
      )
      chat$chat("Is R a functional programming language?")
    }
