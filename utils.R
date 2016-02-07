normalize_arabic <- function(x) {
  text_temp <- x
  text_temp <- gsub("\\p{P}", " ", text_temp, perl = TRUE) # Remove punctuation
  # Remove leading whitespace, remove extra spaces, remove non-letter, non-space characters
  text_temp <- gsub('^ ', '', stripWhitespace(gsub('[^\\p{L}\\p{Zs}]', '', text_temp, perl = TRUE)))
  text_temp <- stripWhitespace(gsub('\\x{0623}|\\x{0622}|\\x{0625}|\\x{0671}|\\x{0672}|\\x{0673}', 'ا', text_temp)) 
  # Normalize alefs with hamzas
  text_temp <- gsub('\\p{Zs}\\x{0648}*\\x{0627}\\x{0644}(?=\\p{L})', ' ', text_temp, perl = TRUE) 
  # Remove leading alef lam with optional leading waw
  text_temp <- gsub('^\\x{0627}\\x{0644}(?=\\p{L})', '', text_temp, perl = TRUE) 
  # Remove leading alef lam at start of string
  text_temp <- gsub('\\p{Zs}\\x{0648}*\\x{0644}{2,}(?=\\p{L})', ' ', text_temp, perl = TRUE) 
  # Remove leading double lam at start of string
  text_temp <- gsub('\\p{Zs}\\x{0648}*\\x{0643}\\x{0627}\\x{0644}(?=\\p{L})', ' ', text_temp, perl = TRUE) 
  # Remove leading kaf alef lam with optional waw
  text_temp <- gsub('\\p{Zs}\\x{0648}*\\x{0628}\\x{0627}\\x{0644}(?=\\p{L})', ' ', text_temp, perl = TRUE) 
  # Remove leading baa alef lam with optional waw
  text_temp <- gsub('\\p{Zs}\\x{0648}*\\x{0641}\\x{0627}\\x{0644}(?=\\p{L})', ' ', text_temp, perl = TRUE) 
  # Remove leading faa alef lam with optional waw
  text_temp <- gsub('\\p{Zs}\\x{0648}*\\x{0627}{2,}\\x{0644}*(?=\\p{L})', ' ', text_temp, perl = TRUE) 
  # Remove leading double alef with optional lam with optional leading waw
  text_temp <- gsub('(?<=\\p{L})\\x{0647}(?=\\p{Zs})', ' ', text_temp, perl = TRUE) 
  # Remove trailing haa
  text_temp <- gsub('(?<=\\p{L})\\x{0649}(?=\\p{Zs})', 'ي', text_temp, perl = TRUE) 
  # Normalize ending yeh
  text_temp <- gsub('(?<=\\p{L})\\x{064A}{2,}\\x{0646}(?=\\p{Zs})', '', text_temp, perl = TRUE) 
  # Remove trailing yeh yeh noon
  text_temp <- gsub('(?<=\\p{L})\\x{064A}\\x{0648}\\x{0646}(?=\\p{Zs})', '', text_temp, perl = TRUE) 
  # Remove trailing yeh waw noon
  text_temp <- gsub('(?<=\\p{L})\\x{0647}\\x{0647}*(?=\\p{Zs})', '', text_temp, perl = TRUE) 
  # Remove trailing haa or haa alef
  text_temp <- gsub('(?<=\\p{L})\\x{0647}\\x{0645}\\x{0627}*(?=\\p{Zs})', '', text_temp, perl = TRUE) 
  # Remove trailing haa meem and haa meem alef
  text_temp <- gsub('(?<=\\p{Zs})\\p{L}(?=\\p{Zs})', '', text_temp, perl = TRUE) 
  # Remove single letters such as waw and those produced by above normalization
  text_temp <- stripWhitespace(gsub('(\\p{Zs}$)|(^\\p{Zs})', '', text_temp, perl = TRUE)) 
  # Remove added, leading, trailing whitespace
  return(text_temp)
}



languages <- read.csv("lang.csv")