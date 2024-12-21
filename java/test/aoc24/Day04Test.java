package aoc24;

import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Day04Test {

    @Test
    void testCountOccurrencesExample1() {
        List<String> grid = Arrays.asList(
                "..X...",
                ".SAMX.",
                ".A..A.",
                "XMAS.S",
                ".X...."
        );
        long expected = 4;
        long actual = Day04.countOccurrences(grid);
        assertEquals(expected, actual);
    }

    @Test
    void testCountOccurrencesExample2() {
        List<String> grid = Arrays.asList(
                "MMMSXXMASM",
                "MSAMXMSMSA",
                "AMXSXMAAMM",
                "MSAMASMSMX",
                "XMASAMXAMM",
                "XXAMMXXAMA",
                "SMSMSASXSS",
                "SAXAMASAAA",
                "MAMMMXMMMM",
                "MXMXAXMASX"
        );
        long expected = 18;
        long actual = Day04.countOccurrences(grid);
        assertEquals(expected, actual);
    }

    @Test
    void testCountXMASShapesExample() {
        List<String> grid = Arrays.asList(
                ".M.S......",
                "..A..MSMS.",
                ".M.S.MAA..",
                "..A.ASMSM.",
                ".M.S.M....",
                "..........",
                "S.S.S.S.S.",
                ".A.A.A.A..",
                "M.M.M.M.M.",
                ".........."
        );
        long expected = 9;
        long actual = Day04.countXMASShapes(grid);
        assertEquals(expected, actual);
    }
}
