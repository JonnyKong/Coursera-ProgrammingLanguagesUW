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
  rotations([[-1, 0], [0, 0], [1, 0], [1, 1], [0, 1]]),  # 5-block blob piece
  [[[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]],            # Long (only needs two)
  [[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]]],
  rotations([[0, 0], [0, 1], [1, 0]])]                    # Small L

  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.next_cheat_piece (board)
    MyPiece.new([[[0, 0]]], board)
  end

end

class MyBoard < Board
  # your enhancements here

  # Initialize with MyPiece other than Piece
  def initialize(game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}  # Create 2D Grid
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheat = false
    # raise "Empty block created" unless !@current_block.nil?
  end

  # Create with MyPiece other that Piece
  def next_piece
    if @cheat
      @current_block = MyPiece.next_cheat_piece(self)
      @cheat = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    
    @current_pos = nil
    # raise "Empty block created" unless !@current_block.nil?
  end

  # Blocks sizes can be other than 4
  def store_current
    # raise "Nil block created" unless !@current_block.nil?
    locations = @current_block.current_rotation
    displacement = @current_block.position
    size = locations.size
    (0..(size - 1)).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def start_cheat
    if @score >= 100 && @cheat == false
      @cheat = true
      @score -= 100
    end
  end

  def rotate_twice
    rotate_clockwise
    rotate_clockwise
  end

end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    # Rotate 180 degrees
    @root.bind('u', proc {@board.rotate_clockwise; @board.rotate_clockwise})  

    # Cheat on next block
    @root.bind('c', proc {@board.start_cheat})  
  end
  
end