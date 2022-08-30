//
//  Day14.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-30.
//

import Foundation

class Day14: AoCSolution {
	var elf1 = 0
	var elf2 = 1
	
	override init() {
		super.init()
		day = 14
		name = "Chocolate Charts"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = Int(AoCUtil.readGroupedInputFile(named: filename, group: index).first!)!
		
		let result1 = solvePartOne(numRecipes: input)
		print("The scores of the next 10 recipes after \(input) are \(result1)")
		
		return AoCResult(part1: result1, part2: nil)
	}
	
	private func solvePartOne(numRecipes: Int) -> String {
		var scoreboard = [3,7]
		elf1 = 0
		elf2 = 1
		
		printScoreboard(scoreboard)
		while scoreboard.count < numRecipes + 10 {
			makeRecipes(&scoreboard)
			printScoreboard(scoreboard)
		}
		
		let lastTen = scoreboard[numRecipes..<(numRecipes+10)]
		assert(lastTen.count == 10)
		let result = (lastTen.map {String($0)}).joined()
		return result
	}
	
	private func makeRecipes(_ scoreboard: inout [Int]) {
		let sum = scoreboard[elf1] + scoreboard[elf2]
		scoreboard.append(contentsOf: AoCUtil.numberToIntArray(sum))
		
		// Move elves by the score of their current recipe + 1
		for _ in 0...scoreboard[elf1] {
			elf1 += 1
			if elf1 >= scoreboard.count { elf1 = 0 }
		}
		for _ in 0...scoreboard[elf2] {
			elf2 += 1
			if elf2 >= scoreboard.count { elf2 = 0 }
		}
	}
	
	private func printScoreboard(_ scoreboard: [Int]) {
		guard scoreboard.count < 30 else { return }
		
		var temp = [String]()
		for i in 0..<scoreboard.count {
			if i == elf1 		{	temp.append("(\(scoreboard[i]))")}
			else if i == elf2	{	temp.append("[\(scoreboard[i])]")}
			else				{	temp.append(String(scoreboard[i]))}
		}
		
		print(temp.joined(separator: " "))
	}
}
