//
//  Day19.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-09-11.
//

import Foundation

class Day19: AoCSolution {
	override init() {
		super.init()
		day = 19
		name = "Go With The Flow"
	}
	
	override func solve(filename: String, index: Int) -> AoCResult {
		super.solve(filename: filename, index: index)
		let input = AoCUtil.readInputFile(named: filename, removingEmptyLines: true)
		
		let program = parseProgram(input)
		let result1 = solvePart(program, registerZeroValue: 0)
		print("Part One: the final value in register 0 is \(result1)")
		
		let result2 = solvePart(program, registerZeroValue: 1)
		print("Part Two: the final value in register 0 is \(result2)")

		return AoCResult(part1: String(result1), part2: String(result2))
	}
	
	private func solvePart(_ program: Program, registerZeroValue: Int) -> Int {
		var register = [Int](repeating: 0, count: 6)
		register[0] = registerZeroValue
		var ip = 0
		print("[\(register.map({String($0)}).joined(separator: ","))]")
		
		while 0 <= ip && ip < program.instructions.count {
			let instruction = program.instructions[ip]
			
			// When the instruction pointer is bound to a register, its value is written
			// to that register just before each instruction is executed
			register[program.ipRegister] = ip
			
			// Short circuit of the tight inner loop
			// https://www.reddit.com/r/adventofcode/comments/a7j9zc/2018_day_19_solutions/
			/*
			 11: seti 2 6 1  			Set IP to 2
			 [0,2,0,2,10551358,1]
			 3: mulr 5 3 2				Set R2 to R5 * R3
			 [0,3,2,2,10551358,1]
			 4: eqrr 2 4 2				Set R2 to (1:0) R2 eq R4
			 [0,4,0,2,10551358,1]
			 5: addr 2 1 1				Set IP to R2 + IP. i.e. If R3*R5=R4 then instruction 7 increments R0 by R5
			 [0,5,0,2,10551358,1]
			 6: addi 1 1 1				Set IP to IP + 1
			 [0,7,0,2,10551358,1]
			 8: addi 3 1 3				Set R3 to R3 + 1
			 [0,8,0,3,10551358,1]
			 9: gtrr 3 4 2				Set R2 to (1:0) R3 eq R4 // R3 has reached the number we're factoring
			 [0,9,0,3,10551358,1]
			 10: addr 1 2 1				Set IP to IP + R2  // If R2 is not zero, then will not loop to 11
			 [0,10,0,3,10551358,1]
			 */
			if ip == 11 {
				// Need to get the sum of factors of the number in register[4]
				while register[5] <= register[4] {
					let product = register[3] * register[5]
					if product == register[4] {
						register[0] += register[5]
					}
					if product >= register[4] {
						register[3] = 0
						register[5] += 1
					}
					register[3] += 1
				}
				break
			}
			
			instruction.computeInplace(register: &register)

			// and the value of that register is written back to the instruction pointer
			// immediately after each instruction finishes execution
			ip = register[program.ipRegister]
			
			// Afterward, move to the next instruction by adding one to the instruction pointer,
			// even if the value in the instruction pointer was just updated by an instruction
			ip += 1
		}
		
		return register[0]
	}
	
	private func parseProgram(_ input: [String]) -> Program {
		var input = input
		
		// First line sets the instruction pointer
		let ipLine = input.removeFirst()
		let parts = ipLine.split(separator: " ")
		let ip = Int(parts[1])!
		
		// Following lines are instructions
		var instr = [Instruction]()
		for line in input {
			let parts = line.split(separator: " ").map { String($0) }
			let i = Instruction(code: parts[0], values: parts[1...3].map({ Int($0)! }))
			instr.append(i)
		}
		
		return Program(ipRegister: ip, instructions: instr)
	}
}

private struct Program {
	var ipRegister: Int
	let instructions: [Instruction]
}

private struct Instruction: CustomDebugStringConvertible {
	let opCode: OpCodeType
	let A: Int
	let B: Int
	let C: Int
	
	init(code: String, values: [Int]) {
		opCode = OpCodeType(rawValue: code)!
		A = values[0]
		B = values[1]
		C = values[2]
	}
	
	var debugDescription: String { return description }
	var description: String {
		return "\(opCode) \(A) \(B) \(C)"
	}
	
	func computeInplace(register: inout [Int]) {
		switch opCode {
		case .addr:
			register[C] = register[A] + register[B]
		case .addi:
			register[C] = register[A] + B
		case .mulr:
			register[C] = register[A] * register[B]
		case .muli:
			register[C] = register[A] * B
		case .banr:
			register[C] = register[A] & register[B]
		case .bani:
			register[C] = register[A] & B
		case .borr:
			register[C] = register[A] | register[B]
		case .bori:
			register[C] = register[A] | B
		case .setr:
			register[C] = register[A]
		case .seti:
			register[C] = A
		case .gtir:
			register[C] = (A > register[B]) ? 1 : 0
		case .gtri:
			register[C] = (register[A] > B) ? 1 : 0
		case .gtrr:
			register[C] = (register[A] > register[B]) ? 1 : 0
		case .eqir:
			register[C] = (A == register[B]) ? 1 : 0
		case .eqri:
			register[C] = (register[A] == B) ? 1 : 0
		case .eqrr:
			register[C] = (register[A] == register[B]) ? 1 : 0
		}
	}
	
	func compute(input register: [Int]) -> [Int] {
		var output = register // Default, no-op
		
		computeInplace(register: &output)
		return output
	}
}

private enum OpCodeType: String, CaseIterable {
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
