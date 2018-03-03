# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
    # The constant All_My_Pieces should be declared here
      All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                     rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                     [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                     [[0, 0], [0, -1], [0, 1], [0, 2]]],
                     rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                     rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                     rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                     rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                     rotations([[0, 0], [-1, 0], [-1, 1], [0, 1],  [1,1]]),
                     rotations([[0, 0], [0, 1], [1, 0]]),
                     [[[0,0], [0,1], [0,2], [0,-1], [0,-2]],
                     [[0,0], [1,0], [2,0], [-1,0], [-2,0]]]]
      Cheat_Piece = [[[0,0]]]
    # your enhancements here
    # delete
    def initialize (point_array, board)
      @all_rotations = point_array
      @rotation_index = (0..(@all_rotations.size-1)).to_a.sample
      @color = All_Colors.sample
      @base_position = [5, 0] # [column, row]
      @board = board
      @moved = true
    end
  
    def self.next_piece (board)
      MyPiece.new(All_My_Pieces.sample, board)
    end
  
    def self.new_cheat_piece(board)
      MyPiece.new(Cheat_Piece, board)
    end
  
  end
  
  class MyBoard < Board
    # your enhancements here
  
    def initialize (game)
      super
      @current_block = MyPiece.next_piece(self)
      @cheated = false
    end
  
    # gets the next piece
    def next_piece
      if @cheated
          @cheated = false
          @current_block = MyPiece.new_cheat_piece(self)
      else
          @current_block = MyPiece.next_piece(self)
      end
      @current_pos = nil
    end
  
    def rotate180degrees
        if !game_over? and @game.is_running?
        @current_block.move(0, 0, 2)
      end
      draw
    end
  
    def store_current
      locations = @current_block.current_rotation
      displacement = @current_block.position
      (0..(locations.size - 1)).each{|index| 
        current = locations[index];
        @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
        @current_pos[index]
      }
      remove_filled
      @delay = [@delay - 2, 80].max
    end
  
    def cheat
      if !@cheated && score >= 100
          @score = score - 100
          @cheated = true
      end
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
        @root.bind('u', proc {@board.rotate180degrees})
      @root.bind('c', proc {@board.cheat})
    end
  end
  
  
  