package aoc24;

import org.junit.jupiter.api.Test;

import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class Day05Test {

    @Test
    void testMiddlePages() {
        assertEquals(Arrays.asList(61L), Day05.middlePages(Day05.parseUpdate("75,47,61,53,29")));
        assertEquals(Arrays.asList(53L), Day05.middlePages(Day05.parseUpdate("97,61,53,29,13")));
        assertEquals(Arrays.asList(29L), Day05.middlePages(Day05.parseUpdate("75,29,13")));
    }
}
