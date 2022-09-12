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
		var countIterations = 0
		
		while 0 <= ip && ip < program.instructions.count {
			let instruction = program.instructions[ip]
			
			// When the instruction pointer is bound to a register, its value is written
			// to that register just before each instruction is executed
			register[program.ipRegister] = ip
			
			instruction.computeInplace(register: &register)

			// and the value of that register is written back to the instruction pointer
			// immediately after each instruction finishes execution
			ip = register[program.ipRegister]
			
//			if ip + 1 < 0 || program.instructions.count < ip + 1 {
//				print("ip=\(ip) [\(register.map({String($0)}).joined(separator: ","))] \(instruction) [\(result.map({String($0)}).joined(separator: ","))]")
//			}
			
			// Afterward, move to the next instruction by adding one to the instruction pointer,
			// even if the value in the instruction pointer was just updated by an instruction
			ip += 1

			//register = result
			countIterations += 1
			if countIterations % 100000 == 0 {
				print(countIterations)
			}
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
