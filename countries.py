import pycountry

countries = [country.name for country in pycountry.countries]
print(len(countries))  # Should output: 249
