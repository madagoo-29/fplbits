require(RSelenium)
require(rJava)

selenium_server <- rsDriver(
  browser = "chrome",
  chromever = "latest"
)
