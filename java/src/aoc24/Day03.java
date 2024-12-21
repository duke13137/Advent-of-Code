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

    private static int[] findNextInstruction(String input) {
        int doIndex = input.indexOf("do()");
        int dontIndex = input.indexOf("don't()");

        if (doIndex == -1 && dontIndex == -1) {
            return null;
        } else if (doIndex == -1) {
            return new int[]{1, dontIndex};
        } else if (dontIndex == -1) {
            return new int[]{0, doIndex};
        } else if (doIndex < dontIndex) {
            return new int[]{0, doIndex};
        } else {
            return new int[]{1, dontIndex};
        }
    }

    private static long[] processChunk(String input, boolean enabled, long sum) {
        int[] instruction = findNextInstruction(input);
        if (instruction != null) {
            int type = instruction[0];
            int index = instruction[1];
            String chunk = input.substring(0, index);
            String remainingInput = input.substring(index + (type == 0 ? 4 : 6));
            return new long[]{sum + executeMul(chunk, enabled), type == 0 ? 1 : 0, remainingInput.length()};
        } else {
            return new long[]{sum + executeMul(input, enabled), enabled ? 1 : 0, 0};
        }
    }

    public static long part2(String input) {
        long sum = 0;
        boolean enabled = true;
        while (input.length() > 0) {
            long[] result = processChunk(input, enabled, sum);
            sum = result[0];
            enabled = result[1] == 1;
            if (result[2] == 0) {
                break;
            }
            input = input.substring((int) (input.length() - result[2]));
        }
        return sum;
    }
}
