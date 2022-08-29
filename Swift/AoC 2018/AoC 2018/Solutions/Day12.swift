//
//  Day12.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-28.
//

import Foundation

class Day12: AoCSolution {
	override init() {
		super.init()
		day = 12
		name = "Subterranean Sustainability"
		self.emptyLinesIndicateMultipleInputs = false
	}

	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: false)
		let parsed = parseInput(input)
		
		let result1 = process(state: parsed.state, liveRules: parsed.live, dieRules: parsed.die, iterations: 20)
		print("Part One: the sum of the pot numbers with a plant is \(result1)")
		
		let firstThousandScore =  process(state: parsed.state, liveRules: parsed.live, dieRules: parsed.die, iterations: 1000)
		let secondThousandScore = process(state: parsed.state, liveRules: parsed.live, dieRules: parsed.die, iterations: 2000)
		let scorePerThousand = secondThousandScore - firstThousandScore

		var result2 = 50000000 * scorePerThousand
		result2 = result2 - scorePerThousand + firstThousandScore
		print("Part Two: total score for fifty billion iterations is \(result2)")

		return AoCResult(part1: String(result1), part2: String(result2))
	}
	
	private func process(state original: String, liveRules: [Rule], dieRules: [Rule], iterations: Int) -> Int {
		//Pad with "."
		let PAD = "....."
		let TOO_MUCH_PAD = "...................."
		var state = PAD + original + PAD
		var leftOffset = -5
		
		//print("0: \(state)")
		for _ in 1...iterations {
			// Assume dead, don't need to process dieRules?
			var newState = state.replacingOccurrences(of: "#", with: ".")
			
			for rule in liveRules {
				for index in state.indicesOf(string: rule.pattern) {
					//print("Rule \(rule.pattern) matches at \(index)")
					newState[index+2] = "#" // We are setting the centre of the pattern, hence +2
				}
			}
			
			state = newState
			if state.starts(with: TOO_MUCH_PAD) {
				state = String(state.dropFirst(5))
				leftOffset += 5
			}
			if !state.hasPrefix(PAD) {
				state = PAD + state
				leftOffset -= 5
			}
			if !state.hasSuffix(PAD) {
				state.append(PAD)
			}
			//print("\(iter): \(state)")
		}
		
		var score = 0
		for (i, c) in Array(state).enumerated() {
			if c == "#" {
				score += i + leftOffset
			}
		}
		return score;
	}
	
	private func parseInput(_ input: [String]) -> (state: String, live: [Rule], die: [Rule]) {
		let re = NSRegularExpression("([#\\.]+)")
		
		var matches = re.allMatches(input[0]) // re.positionalMatches(input[0])
		let state = matches[0]
		
		var liveRules = [Rule]()
		var dieRules = [Rule]()
		
		for i in 2..<input.count {
			matches = re.allMatches(input[i])
			let r = Rule(pattern: matches[0])
			if matches[1] == "#" {
				liveRules.append(r)
			}
			else {
				dieRules.append(r)
			}
		}
		return (state: state, live: liveRules, die: dieRules)
	}
}

private struct Rule {
	let pattern: String
}
