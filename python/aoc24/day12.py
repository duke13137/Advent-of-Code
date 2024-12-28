from collections import deque


def calculate_region_properties(grid, start_row, start_col, plant_type, visited):
    """Calculates the area and perimeter of a region."""
    rows = len(grid)
    cols = len(grid[0])
    area = 0
    perimeter = 0
    queue = deque([(start_row, start_col)])
    visited.add((start_row, start_col))

    while queue:
        row, col = queue.popleft()
        area += 1

        # Check neighbors
        for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            new_row, new_col = row + dr, col + dc

            # Check boundaries
            if 0 <= new_row < rows and 0 <= new_col < cols:
                if (
                    grid[new_row][new_col] == plant_type
                    and (new_row, new_col) not in visited
                ):
                    queue.append((new_row, new_col))
                    visited.add((new_row, new_col))
                elif grid[new_row][new_col] != plant_type:
                    perimeter += 1
            else:
                # Out of bounds, counts towards perimeter
                perimeter += 1

    return area, perimeter


def calculate_total_price(grid):
    """Calculates the total price of fencing all regions."""
    rows = len(grid)
    cols = len(grid[0])
    visited = set()
    total_price = 0

    for row in range(rows):
        for col in range(cols):
            if (row, col) not in visited:
                plant_type = grid[row][col]
                area, perimeter = calculate_region_properties(
                    grid, row, col, plant_type, visited
                )
                total_price += area * perimeter

    return total_price


# Test cases
example1 = ["AAAA", "BBCD", "BBCC", "EEEC"]

example2 = ["OOOOO", "OXOXO", "OOOOO", "OXOXO", "OOOOO"]

example3 = [
    "RRRRIICCFF",
    "RRRRIICCCF",
    "VVRRRCCFFF",
    "VVRCCCJFFF",
    "VVVVCJJCFE",
    "VVIVCCJJEE",
    "VVIIICJJEE",
    "MIIIIIJJEE",
    "MIIISIJEEE",
    "MMMISSJEEE",
]

print(f"Example 1 Total Price: {calculate_total_price(example1)}")  # Expected: 140
print(f"Example 2 Total Price: {calculate_total_price(example2)}")  # Expected: 772
print(f"Example 3 Total Price: {calculate_total_price(example3)}")  # Expected: 1930
