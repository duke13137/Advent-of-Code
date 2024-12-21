package aoc24;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day03 {

    public static void main(String[] args) throws IOException {
        Path inputPath = Path.of("resources", "aoc24", "day03.txt");
        String input = Files.readString(inputPath).trim();

        long part1Result = part1(input);
        System.out.println("Part 1: " + part1Result);

        long part2Result = part2(input);
        System.out.println("Part 2: " + part2Result);
    }

    public static long executeMul(String input, boolean enabled) {
        Pattern pattern = Pattern.compile("mul\\((\\d+),(\\d+)\\)");
        Matcher matcher = pattern.matcher(input);
        long sum = 0;
        while (matcher.find()) {
            if (enabled) {
                long a = Long.parseLong(matcher.group(1));
                long b = Long.parseLong(matcher.group(2));
                sum += a * b;
            }
        }
        return sum;
    }

    public static long part1(String input) {
        return executeMul(input, true);
    }

    public static long part2(String input) {
        long sum = 0;
        boolean enabled = true;
        int doIndex = input.indexOf("do()");
        int dontIndex = input.indexOf("don't()");

        while (doIndex != -1 || dontIndex != -1) {
            int nextIndex = -1;
            if (doIndex != -1 && (dontIndex == -1 || doIndex < dontIndex)) {
                nextIndex = doIndex;
            } else if (dontIndex != -1) {
                nextIndex = dontIndex;
            }

            sum += executeMul(input.substring(0, nextIndex), enabled);
            enabled = (nextIndex == doIndex);
            input = input.substring(nextIndex + (nextIndex == doIndex ? 4 : 6));

            doIndex = input.indexOf("do()");
            dontIndex = input.indexOf("don't()");
        }

        sum += executeMul(input, enabled);
        return sum;
    }
}
