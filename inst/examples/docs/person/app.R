pkgload::load_all()

# example package data
data("df_ll")

# launch person age/sex pyramid module
launch_module(
  module = "person",
  df = df_ll,
  age_var = "age_years",
  sex_var = "sex_id",
  male_level = "Male",
  female_level = "Female"
)
