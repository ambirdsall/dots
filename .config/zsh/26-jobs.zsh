alias j=jobs
# Typing the percent sign gets annoying fast when you run `kill` all the time with `%n`-style arguments on suspended `jobs`
k () {
    kill %"$1"
}
