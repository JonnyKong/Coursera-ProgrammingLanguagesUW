# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
    # The constant All_My_Pieces should be declared here
    All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                  rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                  [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long 4 (only needs two)
                   [[0, 0], [0, -1], [0, 1], [0, 2]]],
                  rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                  rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                  rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                  rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                  rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [1, -1]]), # @@
                                                                          # @@@
                  [[[0, 0], [-1, 0], [1, 0], [2, 0], [-2, 0]], # long 5 (only needs two)
                   [[0, 0], [0, -1], [0, 1], [0, 2], [0, -2]]],
                  rotations([[0, 0], [-1, 0], [0, -1]])] # small L
    # your enhancements here
  
    # block size
    def current_size
      @all_rotations[@rotation_index].length
    end
  
    def self.next_piece (board, cheat = false)
      if cheat
        MyPiece.new([[[0, 0]]], board) # Make a cheat block.
      else
        MyPiece.new(All_My_Pieces.sample, board)
      end
    end
  end
  
  class MyBoard < Board
    # your enhancements here
  
    def initialize (game)
      super
      @current_block = MyPiece.next_piece(self)
      @cheat_status = false # Flag of cheat status.
    end
  
    def rotate_180 # Rotate 180 degrees equal normally rotate twice.
      self.rotate_counter_clockwise
      self.rotate_counter_clockwise
      self.draw
    end
  
    def cheat # Change the cheat flag iif score >= 100 and cheat flag is false to prevent
              # subtract multiple times.
      if (score >= 100) && !@cheat_status
        @cheat_status = true
        @score = score - 100
      end
    end
  
    # gets the next piece
    def next_piece
      if @cheat_status
        @current_block = MyPiece.next_piece(self, true)
        @cheat_status = false # Reset cheat flag after cheat piece appears.
      else
        @current_block = MyPiece.next_piece(self)
      end
      @current_pos = nil
    end
  
    def store_current
      locations = @current_block.current_rotation
      displacement = @current_block.position
      (0..@current_block.current_size-1).each{|index| # modify to adapt different block size.
        current = locations[index];
        @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
            @current_pos[index]
      }
      remove_filled
      @delay = [@delay - 2, 80].max
    end
  end
  
  class MyTetris < Tetris
    # your enhancements here
    def key_bindings
      super
      @root.bind('u', proc {@board.rotate_180})
      @root.bind('c', proc {@board.cheat})
    end
  
    def set_board
      @canvas = TetrisCanvas.new
      @board = MyBoard.new(self)
      @canvas.place(@board.block_size * @board.num_rows + 3,
                    @board.block_size * @board.num_columns + 6, 24, 80)
      @board.draw
    end
  end
  