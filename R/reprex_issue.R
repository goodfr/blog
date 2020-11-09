dfr <- tibble::tribble(
	~var1,          ~var2, ~var3,
	"gr¦pefruit",   "ugli fruit",    4L,
	"huckleberry",      "kumquat",    6L,
	"duri¦n", "blackcurrant",    1L,
	"d¦te",   "cantaloup¦",    1L,
	"fig", "canary m¦lon",    6L,
	"s¦l¦l berry",     "rambutan",    5L
)

# Method 1 - base R
dfr_clean <- dfr
dfr_clean$var1 <- gsub(pattern = "¦", replacement = "a", x = dfr$var1)
dfr_clean[["var2"]] <- gsub(pattern = "¦", replacement = "e", x = dfr$var2)

# Method 2 - tidyverse
library(dplyr)
library(stringr)
dfr %>% mutate(var1 = str_replace_all(string = var1, pattern = "¦", replacement = "a"),
	       var2 = str_replace_all(string = var2, pattern = "¦", replacement = "e")
)

reprex::reprex(si = TRUE, venue = "so")


head(iris)

reprex::reprex(x = {
	dfr <- tibble::tribble(
		~var1,          ~var2, ~var3,
		"grpefruit",   "ugli fruit",    4L,
		"huckleberry",      "kumquat",    6L,
		"duri n", "blackcurrant",    1L,
		"d te",   "cantaloup ",    1L,
		"fig", "canary m lon",    6L,
		"s l l berry",     "rambutan",    5L
	)
	
	# Method 1 - base R
	dfr_clean <- dfr
	dfr_clean$var1 <- gsub(pattern = " ", replacement = "a", x = dfr$var1)
	dfr_clean[["var2"]] <- gsub(pattern = " ", replacement = "e", x = dfr$var2)
	
	# Method 2 - tidyverse
	# library(dplyr)
	# library(stringr)
	# dfr %>% mutate(
		# var1 = str_replace_all(string = var1, pattern = " ", replacement = "a"),
		       # var2 = str_replace_all(string = var2, pattern = " ", replacement = "e")
	# )
},
	       si = TRUE, venue = "so")



reprex::reprex(x = {
	tibble::tribble(
		~var1,          ~var2, ~var3,
		"gr¦pefruit",   "ugli fruit",    4L,
		"huckleberry",      "kumquat",    6L,
		"duri¦n", "blackcurrant",    1L,
		"d¦te",   "cantaloup¦",    1L,
		"fig", "canary m¦lon",    6L,
		"s¦l¦l berry",     "rambutan",    5L
	)
}, si = TRUE)

stringr::str_replace_all("gr\xe9pefruit", "\xe9", "e")


reprex::reprex(x = {
	tibble::tribble(
		~var1,          ~var2, ~var3,
		"gr\xa6pefruit",   "ugli fruit",    4L,
		"huckleberry",      "kumquat",    6L,
		"duri\xa6n", "blackcurrant",    1L,
		"d\xa6te",   "cantaloup\xa6",    1L,
		"fig", "canary m\xa6lon",    6L,
		"s\xa6l\xa6l berry",     "rambutan",    5L
	)
}, si = TRUE)

sessionInfo(
	
)



# answer ---

reprex_locale <- function(...,
			  language = "en",
			  locale = NULL) {
	withr::local_envvar(c(LANGUAGE = language))
	if (!is.null(locale)) {
		# If we use withr::local_locale(), the new locale is NOT inherited by the
		# reprexing child process. Whereas it is if we use an env var approach.
		withr::local_envvar(locale)
	}
	reprex::reprex(...)
}

reprex_locale(x = {
	tibble::tribble(
		~var1,          ~var2, ~var3,
		"gr¦pefruit",   "ugli fruit",    4L,
		"huckleberry",      "kumquat",    6L,
		"duri¦n", "blackcurrant",    1L,
		"d¦te",   "cantaloup¦",    1L,
		"fig", "canary m¦lon",    6L,
		"s¦l¦l berry",     "rambutan",    5L
	)}
)
# ?withr::local_envvar
# Sys.getlocale()

withr::local_envvar(c("LC_COLLATE" = "French_France.1252"))
