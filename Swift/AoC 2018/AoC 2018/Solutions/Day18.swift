//
//  Day18.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-09-09.
//

import Foundation
import Algorithms

class Day18: AoCSolution {
	override init() {
		super.init()
		day = 18
		name = "Settlers of The North Pole"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		let map = parseMap(input)
		
		let result1 = solvePartOne(map)
		print("Part One: the number of woods * number of lumberyards is \(result1)")
				
		let result2 = solvePartTwo(map)
		print("Part Two: the result after 1000000000 minutes is \(result2)")
		
		return AoCResult(part1: String(result1), part2: String(result2))
	}
	
	private func solvePartOne(_ map: AoCGrid2D) -> Int {
		var map = map
		for _ in 1...10 {
			map = playGameOfWoodRound(map)
			//map.draw()
		}
		let lumberyardsCount = map.getCoords(withValue: "#").count
		let woodsCount = map.getCoords(withValue: "|").count
		
		return lumberyardsCount * woodsCount
	}
	
	private func solvePartTwo(_ map: AoCGrid2D) -> Int {
		var map = map
		
		// Have determined that the challenge has a repeating period of 28 minutes
		var valueForMod = [Int](repeating: 0, count: 28)
		// The repeating pattern is established by minute 500
		for i in 1...500 {
			map = playGameOfWoodRound(map)
			let lumberCount = map.getCoords(withValue: "#").count
			let woodsCount = map.getCoords(withValue: "|").count
			let result = lumberCount * woodsCount
			valueForMod[i % 28] = result
			//print(String(format: "%10d % 28 = %2d  . %4d   | %4d   # %4d -> %8d", i, i % 28, stats["."]!, stats["|"]!,stats["#"]!, result))
		}
		
		let mod28 = 1000000000 % 28
		
		return valueForMod[mod28]
	}
	
	private func playGameOfWoodRound(_ map: AoCGrid2D) -> AoCGrid2D {
		let newMap = AoCGrid2D()
		newMap.neighbourRule = .queen
		let ext = map.extent
		
		for (r,c) in product(ext.min.y...ext.max.y, ext.min.x...ext.max.x) {
			let pos = AoCCoord2D(x: c, y: r)
			let currentState = map.value(at: pos)
			
			let neighbours = map.neighbourCoords(at: pos).filter { ext.contains($0) }
			var lumberCount = 0
			var woodsCount = 0
			for n in neighbours {
				let value = map.value(at: n)
				if value == "#" { lumberCount += 1 }
				else if value == "|" { woodsCount += 1 }
			}
			
			if currentState == "." {
				// filled with trees if three or more adjacent acres contained trees
				newMap.setValue(woodsCount >= 3 ? "|" : currentState, at: pos)
			}
			else if currentState == "|" {
				// will become a lumberyard if three or more adjacent acres were lumberyards
				newMap.setValue(lumberCount >= 3 ? "#" : currentState, at: pos)
			}
			else if currentState == "#" {
				// will remain a lumberyard if it was adjacent to at least one other lumberyard and at least one acre containing trees. Otherwise, it becomes open.
				if lumberCount >= 1 && woodsCount >= 1 {
					newMap.setValue(currentState, at: pos)
				}
				else {
					newMap.setValue(".", at: pos)
				}
			}
		}
		return newMap
	}
	
	private func parseMap(_ input: [String]) -> AoCGrid2D {
		let map = AoCGrid2D()
		map.neighbourRule = .queen
		
		for (r,c) in product(0..<input.count, 0..<input[0].count) {
			let char = String(input[r][c])
			map.setValue(char, at: AoCCoord2D(x: c, y: r))
		}
		
		//map.draw()
		
		return map
	}
}
