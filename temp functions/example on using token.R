#1. add secrets folder to repo
#2. add secrets folder to gitignore
#3. Create script in secrets folder that creates token.rda
#4. Load in token.rda in API call script.

# In secret script
token <- "123456"
save(token, file = "secrets/token.rda")

# In API script
load("secrets/token.rda")

metadata <- ga_api_metadata(token = token, synthesis_id = 14)