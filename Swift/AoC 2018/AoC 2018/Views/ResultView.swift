//
//  ResultView.swift
//  AoC 2018
//
//  Created by Simon Biickert on 2022-08-04.
//

import SwiftUI
import OSLog

class SolutionRunner: ObservableObject {
	@Published var running: Bool = false
	@Published var result: AoCResult?
	@Published var elapsed: Int = 0
	
	@MainActor
	func asyncSolve(_ input: AoCInput) async {
		elapsed = 0
		result = input.solution.solve(filename: input.fileName, index: input.index)
		running = false
	}
}

struct ResultView: View {
	var input: AoCInput
	@StateObject private var runner = SolutionRunner()
	
	@ViewBuilder
	var progress: some View {
		if runner.running {
			ProgressView()
		}
		else {
			EmptyView()
		}
	}
	
    var body: some View {
		VStack {
			Button {
				runner.running = true
				let t = Timer.scheduledTimer(withTimeInterval: 0.01, repeats: true) { _ in
					self.runner.elapsed += 10
					//print(String(self.runner.elapsed))
				}
				Task {
					await runner.asyncSolve(input)
					//t.invalidate()
				}
			} label: {
				Text("Solve Day \(input.solution.day)")
			}
			.disabled(runner.running)
			.padding()
			
			VStack(alignment: .leading) {
				Text("Part One").bold()
				HStack {
					Image(systemName: runner.result?.part1 != nil ? "checkmark.seal" : "questionmark.diamond")
						.resizable(capInsets: EdgeInsets(top: 0.0, leading: 0.0, bottom: 0.0, trailing: 0.0))
						.foregroundColor(runner.result?.part1 != nil ? Color.green : Color.gray)
						.padding(.all, 5.0)
						.frame(width: 32, height: 32, alignment: .center)
					Text(runner.result?.part1 ?? "No answer")
				}
				Divider()
				Text("Part Two").bold()
				HStack {
					Image(systemName: runner.result?.part2 != nil ? "checkmark.seal" : "questionmark.diamond")
						.resizable(capInsets: EdgeInsets(top: 0.0, leading: 0.0, bottom: 0.0, trailing: 0.0))
						.foregroundColor(runner.result?.part2 != nil ? Color.green : Color.gray)
						.padding(.all, 5.0)
						.frame(width: 32, height: 32, alignment: .center)
					Text(runner.result?.part2 ?? "No answer")
				}
			}
			StopwatchView(progressTime: $runner.elapsed)
		}
		.padding()
		.overlay(progress)
   }
}

struct ResultView_Previews: PreviewProvider {
    static var previews: some View {
		ResultView(input: inputs(for: solutions[2])[0])
    }
}
