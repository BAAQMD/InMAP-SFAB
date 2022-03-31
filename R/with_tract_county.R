with_tract_county <- function (tract_geodata) {
  mutate(
    tract_geodata,
    cnty_name = codec::decode(
      str_sub(tract_id, 3, 5),
      SFBA::SFBA_COUNTY_FIPS_CODES))
}
