package models

import common.Edition
import common.editions.Us
import org.joda.time.DateTime
import play.api.libs.json.Json

object ForecastResponse {
  implicit val jsonWrites = Json.writes[ForecastResponse]

  def fromAccuweather(forecastResponse: accuweather.ForecastResponse): ForecastResponse = {
    ForecastResponse(
      forecastResponse.EpochDateTime * 1000,
      forecastResponse.WeatherIcon,
      forecastResponse.IconPhrase,
      forecastResponse.Temperature.Unit match {
        case "C" => Temperatures.fromCelsius(forecastResponse.Temperature.Value)
        case "F" => Temperatures.fromFahrenheit(forecastResponse.Temperature.Value)
        case _ =>
          throw new RuntimeException("Temperature of neither celsius nor fahrenheit from " +
            s"Accuweather! $forecastResponse")
      }
    )
  }
}

case class ForecastResponse(
  epochDateTime: Int,
  weatherIcon: Int,
  weatherText: String,
  temperature: Temperatures
) {
  def temperatureForEdition(edition: Edition) = {
    edition match {
      case Us => s"${temperature.imperial.round}°F"
      case _ => s"${temperature.metric.round}°C"
    }
  }

  def hourString = new DateTime(epochDateTime).toString("HH:00")
}
