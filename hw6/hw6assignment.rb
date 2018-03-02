# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces =  
  [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
  rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
  [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
  [[0, 0], [0, -1], [0, 1], [0, 2]]],
  rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
  rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
  rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
  rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
  rotations([[-1, 0], [0, 0], [1, 0], [-1, 1], [0, 1]]),  # First
  [[[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]],            # Long (only needs two)
  [[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]]],
  rotations([[0, 0], [0, 1], [1, 0]])]                    # Small L

  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
    puts "Calling new next_piece"
  end

end

class MyBoard < Board
  # your enhancements here
  def initialize(game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}  # Create 2D Grid
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil
  end

end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    super
    @board = MyBoard.new(self)
  end
  
end


