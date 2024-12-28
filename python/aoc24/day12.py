def find_regions(grid):
    rows = len(grid)
    cols = len(grid[0]) if rows > 0 else 0
    visited = set()
    regions = {}

    def dfs(r, c, plant_type, region):
        if (
            r < 0
            or r >= rows
            or c < 0
            or c >= cols
            or (r, c) in visited
            or grid[r][c] != plant_type
        ):
            return
        visited.add((r, c))
        region.append((r, c))
        dfs(r + 1, c, plant_type, region)
        dfs(r - 1, c, plant_type, region)
        dfs(r, c + 1, plant_type, region)
        dfs(r, c - 1, plant_type, region)

    for r in range(rows):
        for c in range(cols):
            if (r, c) not in visited:
                plant_type = grid[r][c]
                region = []
                dfs(r, c, plant_type, region)
                if region:
                    regions[plant_type] = regions.get(plant_type, []) + [region]
    return regions


def calculate_sides(region, grid):
    rows = len(grid)
    cols = len(grid[0]) if rows > 0 else 0
    sides = 0

    for r, c in region:
        # Check all four directions
        for dr, dc in [(0, 1), (0, -1), (1, 0), (-1, 0)]:
            nr, nc = r + dr, c + dc
            if nr < 0 or nr >= rows or nc < 0 or nc >= cols:  # Edge of grid
                sides += 1
            elif (nr, nc) not in region:
                sides += 1  # Different plant type or not in region
    return sides


def calculate_total_price_with_sides(grid):
    regions = find_regions(grid)
    total_price = 0
    for plant_type, region_list in regions.items():
        for region in region_list:
            area = len(region)
            sides = calculate_sides(region, grid)
            total_price += area * sides
    return total_price


# Example Usage
grid1 = ["AAAA", "BBCD", "BBCC", "EEEC"]

grid2 = [
    "XXXXXXXXXXXXXXXX",
    "XXXXXXXXXXOOOOXX",
    "XXXXXXXOOOXXXXXX",
    "XXXXXXOOXXXXXXXO",
    "XXXXXOOOXXXXXXOO",
    "XXXXXXXXOOOXXXXX",
    "XXXXXXXXXOOXXXXX",
    "XXXXXXXXXXOOXXXX",
    "XXXXXXXXXXOOOXXX",
    "XXXXXXXXXXXXXXXO",
    "XXXXXXXXXXXXXXXX",
]

grid3 = ["EEEEE", "EXXXX", "EEEEE", "EXXXX", "EEEEE"]

grid4 = ["AAAAAA", "AAABBA", "AAABBA", "ABBAAA", "ABBAAA", "AAAAAA"]

grid5 = [
    "RRRRRRRRRRRR",
    "RIIIIIIIIRRR",
    "RIICCCCCCCCC",
    "RIICFFFFCCCC",
    "RIIICFFFFVCC",
    "RIICFFFFVVCC",
    "RIICFFFFVVCC",
    "RIIIJJJJVVCC",
    "RRJJJJJJEEC",
    "RRJJJEEEIICC",
    "RRMSSSSIIIII",
    "RRRRRRRRRRRR",
]


print(f"Price grid 1: {calculate_total_price_with_sides(grid1)}")  # Output: 80
print(f"Price grid 2: {calculate_total_price_with_sides(grid2)}")  # Output: 436
print(f"Price grid 3: {calculate_total_price_with_sides(grid3)}")  # Output: 236
print(f"Price grid 4: {calculate_total_price_with_sides(grid4)}")  # Output: 368
