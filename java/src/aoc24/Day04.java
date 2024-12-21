package aoc24;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

public class Day04 {

  public static void main(String[] args) throws IOException {
    Path inputPath = Path.of("resources", "aoc24", "day04.txt");
    List<String> input = Files.readAllLines(inputPath);

    long part1Result = countOccurrences(input);
    System.out.println("Part 1: " + part1Result);

    long part2Result = countXMASShapes(input);
    System.out.println("Part 2: " + part2Result);
  }

  public static long countOccurrences(List<String> grid) {
    int rows = grid.size();
    int cols = grid.get(0).length();
    String word = "XMAS";
    int wordLen = word.length();
    long count = 0;

    for (int row = 0; row < rows; row++) {
      for (int col = 0; col < cols; col++) {
        count += checkDirections(grid, row, col, word, wordLen);
      }
    }

    return count;
  }

  private static int checkDirections(List<String> grid, int row, int col, String word, int wordLen) {
    int count = 0;
    int[][] directions = { { 0, 1 }, { 1, 0 }, { 1, 1 }, { 1, -1 } }; // right, down, diag-down-right, diag-down-left

    for (int[] dir : directions) {
      int dirRow = dir[0];
      int dirCol = dir[1];
      String forward = extractString(grid, row, col, wordLen, dirRow, dirCol);
      String backward = new StringBuilder(forward).reverse().toString();
      if (forward.equals(word))
        count++;
      if (backward.equals(word))
        count++;
    }
    return count;
  }

  private static String extractString(List<String> grid, int row, int col, int wordLen, int dirRow, int dirCol) {
    StringBuilder sb = new StringBuilder();
    int rows = grid.size();
    int cols = grid.get(0).length();

    for (int i = 0; i < wordLen; i++) {
      int newRow = row + i * dirRow;
      int newCol = col + i * dirCol;

      if (newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols) {
        sb.append(grid.get(newRow).charAt(newCol));
      } else {
        return "";
      }
    }
    return sb.toString();
  }

  public static long countXMASShapes(List<String> grid) {
    int rows = grid.size();
    int cols = grid.get(0).length();
    long count = 0;

    for (int row = 0; row < rows; row++) {
      for (int col = 0; col < cols; col++) {
        if (isXMASShape(grid, row, col)) {
          count++;
        }
      }
    }

    return count;
  }

  private static boolean isXMASShape(List<String> grid, int row, int col) {
    // Check for "MAS" in both diagonal directions and ensure they intersect at a common point
    int[] mas1 = checkDiagonalForMAS(grid, row, col, 1, 1);
    int[] mas2 = checkDiagonalForMAS(grid, row, col, -1, 1);

    if (mas1 != null && mas2 != null) {
      // Check if the two "MAS" sequences intersect at a common point
      return mas1[0] + mas1[2] == mas2[0] && mas1[1] - mas1[2] == mas2[1];
    }

    return false;
  }

  private static int[] checkDiagonalForMAS(List<String> grid, int row, int col, int dirRow, int dirCol) {
    // Check for "MAS" in one diagonal direction and return the coordinates of the found sequence
    String word = "MAS";
    int wordLen = word.length();
    int rows = grid.size();
    int cols = grid.get(0).length();

    for (int startRow = row - 2; startRow <= row + 2; startRow++) {
      for (int startCol = col - 2; startCol <= col + 2; startCol++) {
        if (startRow >= 0 && startRow < rows && startCol >= 0 && startCol < cols) {
          String forward = extractString(grid, startRow, startCol, wordLen, dirRow, dirCol);
          if (forward.equals(word) || new StringBuilder(forward).reverse().toString().equals(word)) {
            // Calculate the end position of "MAS"
            int endRow = startRow + (wordLen - 1) * dirRow;
            int endCol = startCol + (wordLen - 1) * dirCol;
            return new int[] { endRow, endCol, wordLen - 1 };
          }
        }
      }
    }

    return null;
  }

  private static boolean checkOppositeDiagonalForMAS(List<String> grid, int row, int col, int dirRow, int dirCol, String word) {
    // Check for "MAS" in the opposite diagonal direction
    int wordLen = word.length();
    int rows = grid.size();
    int cols = grid.get(0).length();

    for (int startRow = row - 2; startRow <= row + 2; startRow++) {
      for (int startCol = col - 2; startCol <= col + 2; startCol++) {
        if (startRow >= 0 && startRow < rows && startCol >= 0 && startCol < cols) {
          String forward = extractString(grid, startRow, startCol, wordLen, -dirRow, dirCol);
          if (forward.equals(word) || new StringBuilder(forward).reverse().toString().equals(word)) {
            return true;
          }
        }
      }
    }

    return false;
  }
}
