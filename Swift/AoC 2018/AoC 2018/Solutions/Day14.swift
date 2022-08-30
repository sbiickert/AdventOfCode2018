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
		
		let input = AoCUtil.readGroupedInputFile(named: filename, group: index).first!
		
		let result1 = solvePartOne(numRecipes: Int(input)!)
		print("The scores of the next 10 recipes after \(input) are \(result1)")
		
		let result2 = solvePartTwo(search: input)
		print("There are \(result2) digits before finding \(input)")
		
		return AoCResult(part1: result1, part2: result2)
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
	
	private func solvePartTwo(search: String) -> String {
		var scoreboard = [3,7]
		elf1 = 0
		elf2 = 1
		print("Searching for \(search)")

		let searchDigits = AoCUtil.numberToIntArray(search)
		var found = false
		var searchFrom = 0
		while !found {
			let oldCount = scoreboard.count
			makeRecipes(&scoreboard)
			let change = scoreboard.count - oldCount // Normally 1, but can be 2
			for c in 0..<change {
				searchFrom = scoreboard.count - searchDigits.count - c
				if searchFrom < 0 {continue}
				let searchTo = searchFrom + searchDigits.count
				found = [Int](scoreboard[searchFrom..<searchTo]) == searchDigits
				if found {break}
			}
			printScoreboard(scoreboard)
		}
		printScoreboard(scoreboard, tailonly: true)

		return String(searchFrom)
	}
	
	private func makeRecipes(_ scoreboard: inout [Int]) {
		let sum = String(scoreboard[elf1] + scoreboard[elf2])
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
	
	private func printScoreboard(_ scoreboard: [Int], tailonly: Bool = false) {
		guard scoreboard.count < 30 || tailonly else { return }
		
		var temp = [String]()
		
		let start = tailonly ? max(0, scoreboard.count - 20) : 0
		
		for i in start..<scoreboard.count {
			if i == elf1 		{	temp.append("(\(scoreboard[i]))")}
			else if i == elf2	{	temp.append("[\(scoreboard[i])]")}
			else				{	temp.append(String(scoreboard[i]))}
		}
		
		print(temp.joined(separator: " "))
	}
}
