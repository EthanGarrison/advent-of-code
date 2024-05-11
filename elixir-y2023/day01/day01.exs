defmodule Day01 do
  @first_reg ~r/.*?(\d)/
  @second_reg ~r/.*(\d)/
  @first_reg_2 ~r/.*?(\d|one|two|three|four|five|six|seven|eight|nine)/
  @last_reg_2 ~r/.*(\d|one|two|three|four|five|six|seven|eight|nine)/

  def num_str_to_int(s) do
    case s do
      "one" -> "1"
      "two" -> "2"
      "three" -> "3"
      "four" -> "4"
      "five" -> "5"
      "six" -> "6"
      "seven" -> "7"
      "eight" -> "8"
      "nine" -> "9"
      _ -> s
    end
  end

  def extract_nums(x) do
    String.to_integer(Enum.join([hd(tl(Regex.run(@first_reg, x))), hd(tl(Regex.run(@second_reg, x)))]))
  end

  def extract_nums_2(x) do
    String.to_integer(Enum.join([
      num_str_to_int(hd(tl(Regex.run(@first_reg_2, x)))),
      num_str_to_int(hd(tl(Regex.run(@last_reg_2, x))))]))
  end

  def part_one(input) do
    Enum.sum(Enum.map(File.stream!(input), fn l -> extract_nums(l) end))
  end

  def part_two(input) do
    Enum.sum(Enum.map(File.stream!(input), fn l -> extract_nums_2(l) end))
  end
end

