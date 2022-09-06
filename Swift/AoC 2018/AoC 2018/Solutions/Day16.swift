//
//  Day16.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-09-06.
//

import Foundation

class Day16: AoCSolution {
	override init() {
		super.init()
		name = "Chronal Classification"
		day = 16
		emptyLinesIndicateMultipleInputs = false
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		
		let input = AoCUtil.readGroupedInputFile(named: filename)
		let samples = parseSamples(input)
		
		let result1 = solvePartOne(samples)
		print("Part One: the number of samples that behave like three or more opcodes is \(result1)")
		
		return AoCResult(part1: String(result1), part2: nil)
	}
	
	private func solvePartOne(_ samples: [Sample]) -> Int {
		let threeOrMore = samples.filter { $0.possibleOpCodes.count >= 3 }
		return threeOrMore.count
	}
	
	private func parseSamples(_ input: [[String]]) -> [Sample] {
		let regex = NSRegularExpression("(\\d+)")
		var samples = [Sample]()
		for group in input {
			if group.count != 3 { continue }  // Test program, ignore
			var numbers = [[Int]]()
			for line in group {
				let fourNumbers = regex.allMatches(line).map { Int($0)! }
				assert(fourNumbers.count == 4)
				numbers.append(fourNumbers)
			}
			let sample = Sample(before: numbers[0], instruction: Instruction(numbers[1]), after: numbers[2])
			samples.append(sample)
		}
		return samples
	}
}

private struct Sample {
	let before: [Int]
	let instruction: Instruction
	let after: [Int]
	
	var possibleOpCodes: [OpCodeType] {
		get {
			var result = [OpCodeType]()
			
			for oct in OpCodeType.allCases {
				let output = instruction.compute(opCodeType: oct, input: before)
				if output == after {
					print("\(before) --> \(oct) --> \(after) is valid.")
					result.append(oct)
				}
			}
			
			return result
		}
	}
}

private struct Instruction {
	let opCode: Int
	let A: Int
	let B: Int
	let C: Int
	
	init(_ values: [Int]) {
		opCode = values[0]
		A = values[1]
		B = values[2]
		C = values[3]
	}
	
	func compute(opCodeType: OpCodeType, input register: [Int]) -> [Int] {
		var output = register // Default, no-op
		
		switch opCodeType {
		case .addr:
			output[C] = register[A] + register[B]
		case .addi:
			output[C] = register[A] + B
		case .mulr:
			output[C] = register[A] * register[B]
		case .muli:
			output[C] = register[A] * B
		case .banr:
			output[C] = register[A] & register[B]
		case .bani:
			output[C] = register[A] & B
		case .borr:
			output[C] = register[A] | register[B]
		case .bori:
			output[C] = register[A] | B
		case .setr:
			output[C] = register[A]
		case .seti:
			output[C] = A
		case .gtir:
			output[C] = (A > register[B]) ? 1 : 0
		case .gtri:
			output[C] = (register[A] > B) ? 1 : 0
		case .gtrr:
			output[C] = (register[A] > register[B]) ? 1 : 0
		case .eqir:
			output[C] = (A == register[B]) ? 1 : 0
		case .eqri:
			output[C] = (register[A] == B) ? 1 : 0
		case .eqrr:
			output[C] = (register[A] == register[B]) ? 1 : 0
		}
		
		return output
	}
}

private enum OpCodeType: CaseIterable {
	case addr
	case addi
	case mulr
	case muli
	case banr
	case bani
	case borr
	case bori
	case setr
	case seti
	case gtir
	case gtri
	case gtrr
	case eqir
	case eqri
	case eqrr
}
