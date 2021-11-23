package com.pk.logic;

import java.util.List;

import java.util.ArrayList;

import com.pk.logic.exceptions.BadBoardGiven;
import com.pk.logic.exceptions.JumpedOverAlreadyKilledPiece;
import com.pk.logic.exceptions.JumpedOverMoreThanOnePiece;
import com.pk.logic.exceptions.JumpedOverSameColorPiece;
import com.pk.logic.exceptions.MandatoryKillMove;
import com.pk.logic.exceptions.MoreThanOneMoveMade;
import com.pk.logic.exceptions.MoreThanOneTileMove;
import com.pk.logic.exceptions.OverlappingPieces;
import com.pk.logic.exceptions.VerticalOrHorizontalMove;

public class BasicLogic implements Logic {
  public BasicLogic(List<List<Piece>> board) throws BadBoardGiven {
    // fill the board
    if (board.size() < 2 || board.size() % 2 != 0) {
      throw new BadBoardGiven("This format of board is not supported\n");
    }
    this.board = new ArrayList<>(board.size());
    for (int i = 0; i < board.size(); ++i) {
      if (board.get(i).size() < 2 || board.get(i).size() % 2 != 0) {
        throw new BadBoardGiven("This format of board is not supported\n");
      }
      this.board.add(new ArrayList<>(board.get(i)));
    }
    // analyze white and black positions for the first time
    this.white = findAllWhite(board);
    this.black = findAllBlack(board);
  }

  public List<List<Piece>> update(List<List<Piece>> board)
      throws MoreThanOneMoveMade, VerticalOrHorizontalMove, MandatoryKillMove, OverlappingPieces,
      JumpedOverSameColorPiece, JumpedOverMoreThanOnePiece, JumpedOverAlreadyKilledPiece, MoreThanOneTileMove {
    List<PiecePosition> white = findAllWhite(board);
    List<PiecePosition> black = findAllBlack(board);
    // first check for rules violation then we can return proper move
    isNonDiagonalMove(white, black);
    isOverlappingMove(white, black);
    Boolean killMove = wasKillMove();
    isOneMove(white, black, killMove);
    Boolean whiteMove = isWhiteMove(white);
    Integer moveIndex = getMoveIndex(white, black, whiteMove);
    Integer newBoardIndex = moveIndex / this.board.size();
    Integer oldBoardIndex = moveIndex % this.board.size();
    PiecePosition oldPiece;
    PiecePosition newPiece;
    if (whiteMove) {
      oldPiece = this.white.get(oldBoardIndex);
      newPiece = this.white.get(newBoardIndex);
    } else {
      oldPiece = this.black.get(oldBoardIndex);
      newPiece = this.black.get(newBoardIndex);
    }
    isForwardMove(white, black, newBoardIndex, oldBoardIndex, whiteMove, killMove);
    // if it wasn't kill move there is need to check MandatoryKillMove as this
    // condition haven't been accomplished
    if (Boolean.FALSE.equals(killMove)) {
      if (Boolean.TRUE.equals(isKillMoveAvaliable(whiteMove))) {
        throw new MandatoryKillMove();
      } else {
        Boolean isKillMove = isKillMove(board, oldPiece, newPiece, whiteMove);
        // last check from check series
        checkOneTileMove(board, oldPiece, newPiece, whiteMove, isKillMove);
        markKilledPiece(board, oldPiece, newPiece, isKillMove);
        updateBoardWithKings(board, oldPiece, newPiece);
        updateBoardWithNewMove(board, oldPiece, newPiece);
        return this.board;
      }
    }
    return new ArrayList<>(0);
  }

  public List<List<Piece>> updateV2(Integer x, Integer y, Piece affiliation)
      throws MoreThanOneMoveMade, VerticalOrHorizontalMove, MandatoryKillMove, OverlappingPieces,
      JumpedOverSameColorPiece, JumpedOverMoreThanOnePiece, JumpedOverAlreadyKilledPiece, MoreThanOneTileMove {
    return new ArrayList<>(0);
  }

  /**
   * Returns indices of moved pawns. New board pawn is hidden in var +=
   * this.board.size() * index. Old board pawn is hidden in var += index.
   */
  private Integer getMoveIndex(List<PiecePosition> white, List<PiecePosition> black, Boolean whiteMove) {
    Boolean found = false;
    Integer index = -1;
    if (Boolean.TRUE.equals(whiteMove)) {
      for (int i = 0; i < this.white.size(); ++i) {
        for (int j = 0; j < white.size(); ++j) {
          if (this.white.get(i).getX().equals(white.get(j).getX())
              && this.white.get(i).getY().equals(white.get(j).getY())) {
            found = true;
            index = j;
            break;
          }
        }
        if (Boolean.TRUE.equals(found)) {
          found = false;
          index = -1;
        } else {
          return this.board.size() * index + i;
        }
      }
    } else {
      for (int i = 0; i < this.black.size(); ++i) {
        for (int j = 0; j < black.size(); ++j) {
          if (this.black.get(i).getX().equals(black.get(j).getX())
              && this.black.get(i).getY().equals(black.get(j).getY())) {
            found = true;
            index = j;
            break;
          }
        }
        if (Boolean.TRUE.equals(found)) {
          found = false;
          index = -1;
        } else {
          return this.board.size() * index + i;
        }
      }
    }
    throw new RuntimeException();
  }

  private void updateBoardWithKings(List<List<Piece>> board, PiecePosition oldPiece, PiecePosition newPiece) {
    // how pawns are arranged on board
    // y axis will be "final"
    for (int i = 0; i < board.size(); ++i) {
      if (board.get(i).get(board.size() - 1).equals(Piece.WHITE_PAWN)) {
        if (newPiece.getAffiliation().equals(Piece.WHITE_PAWN)) {
          newPiece.setAffiliation(Piece.WHITE_KING);
        } else {
          throw new RuntimeException("Something went wrong with King updates! Oh no");
        }
      }
    }

    for (int i = 0; i < board.size(); ++i) {
      if (board.get(i).get(0).equals(Piece.BLACK_PAWN)) {
        if (newPiece.getAffiliation().equals(Piece.BLACK_PAWN)) {
          newPiece.setAffiliation(Piece.BLACK_KING);
        } else {
          throw new RuntimeException("Something went wrong with King updates! Oh no");
        }
      }
    }
  }

  private void updateBoardWithNewMove(List<List<Piece>> board, PiecePosition oldPiece, PiecePosition newPiece) {
    board.get(oldPiece.getX()).set(oldPiece.getY(), Piece.EMPTY);
    board.get(newPiece.getX()).set(newPiece.getY(), newPiece.getAffiliation());
  }

  // you can take just PiecePositions that changed and compare those, nothing more
  // is needed
  private Boolean isForwardMove(List<PiecePosition> white, List<PiecePosition> black, Integer oldBoardIndex,
      Integer newBoardIndex, Boolean whiteMove, Boolean wasKillMove) {
    Piece indexAffiliation;
    if (Boolean.TRUE.equals(whiteMove)) {
      indexAffiliation = white.get(oldBoardIndex).getAffiliation();
      if (Piece.WHITE_PAWN.equals(indexAffiliation)) {
        return this.white.get(oldBoardIndex).getY() - white.get(newBoardIndex).getY() < 0 || wasKillMove;
      }
    } else {
      indexAffiliation = black.get(oldBoardIndex).getAffiliation();
      if (Piece.BLACK_PAWN.equals(indexAffiliation)) {
        return this.black.get(oldBoardIndex).getY() - black.get(newBoardIndex).getY() > 0 || wasKillMove;
      }
    }
    return false;
  }

  private void markKilledPiece(List<List<Piece>> board, PiecePosition oldPiece, PiecePosition newPiece,
      Boolean isKillMove) {
    Integer axisX = oldPiece.getX().compareTo(newPiece.getX());
    Integer axisY = oldPiece.getY().compareTo(newPiece.getY());
    Piece comparisonPiece;
    if (Boolean.FALSE.equals(isKillMove)) {
      return;
    }
    for (int i = 1; i < Math.abs(oldPiece.getX() - newPiece.getX()); ++i) {
      comparisonPiece = board.get(oldPiece.getX() - (i * axisX)).get(oldPiece.getY() - (i * axisY));
      switch (comparisonPiece) {
      case BLACK_KING:
      case BLACK_PAWN:
        board.get(oldPiece.getX() - (i * axisX)).set(oldPiece.getY() - (i * axisY), Piece.KILLED_BLACK);
        return;
      case WHITE_KING:
      case WHITE_PAWN:
        board.get(oldPiece.getX() - (i * axisX)).set(oldPiece.getY() - (i * axisY), Piece.KILLED_WHITE);
        return;
      case KILLED_BLACK:
      case KILLED_WHITE:
        throw new RuntimeException("Why are we here I ask?");
      case EMPTY:
        break;
      }
    }
  }

  private Boolean isKillMove(List<List<Piece>> board, PiecePosition oldPiece, PiecePosition newPiece, Boolean whiteMove)
      throws JumpedOverSameColorPiece, JumpedOverMoreThanOnePiece, JumpedOverAlreadyKilledPiece {
    Integer axisX = oldPiece.getX().compareTo(newPiece.getX());
    Integer axisY = oldPiece.getY().compareTo(newPiece.getY());
    Piece comparisonPiece;
    Integer killCount = 0;
    if (Boolean.TRUE.equals(whiteMove)) {
      // we can take either X or Y for iteration, it doesn't matter as diagonall move
      // always come with pair of change
      for (int i = 1; i < Math.abs(oldPiece.getX() - newPiece.getX()); ++i) {
        //
        // when oldX is greater than newX for example 7 > 3 then we need to iterate like
        // 7 - i to get to it
        // but in example of different situation we would have to do 3 + i
        // this is solved by doing 7 - (i * change)
        // where change is old > new
        //
        comparisonPiece = board.get(oldPiece.getX() - (i * axisX)).get(oldPiece.getY() - (i * axisY));
        if (comparisonPiece.equals(Piece.EMPTY)) {
          continue;
        } else if (comparisonPiece.equals(Piece.WHITE_KING) || comparisonPiece.equals(Piece.WHITE_PAWN)) {
          throw new JumpedOverSameColorPiece();
        } else if (comparisonPiece.equals(Piece.KILLED_WHITE) || comparisonPiece.equals(Piece.KILLED_BLACK)) {
          throw new JumpedOverAlreadyKilledPiece();
        } else {
          killCount++;
        }
      }
    } else {
      for (int i = 1; i < Math.abs(oldPiece.getX() - newPiece.getX()); ++i) {
        comparisonPiece = board.get(oldPiece.getX() - (i * axisX)).get(oldPiece.getY() - (i * axisY));
        if (comparisonPiece.equals(Piece.EMPTY)) {
          continue;
        } else if (comparisonPiece.equals(Piece.BLACK_KING) || comparisonPiece.equals(Piece.BLACK_PAWN)) {
          throw new JumpedOverSameColorPiece();
        } else if (comparisonPiece.equals(Piece.WHITE_KING) || comparisonPiece.equals(Piece.WHITE_PAWN)) {
          throw new JumpedOverAlreadyKilledPiece();
        } else {
          killCount++;
        }
      }
    }
    if (killCount > 1) {
      throw new JumpedOverMoreThanOnePiece();
    }
    return killCount == 1;
  }

  private void checkOneTileMove(List<List<Piece>> board, PiecePosition oldPiece, PiecePosition newPiece,
      Boolean whiteMove, Boolean killMove) throws MoreThanOneTileMove {
    Integer difference;
    //
    // for kings we can ignore this check, as they can move more than one tile
    // regardless
    //
    if (oldPiece.getAffiliation().equals(Piece.BLACK_KING) || oldPiece.getAffiliation().equals(Piece.WHITE_KING)) {
      return;
    }

    //
    // assuming that move isn't diagonall we can just check difference
    //
    difference = Math.abs(oldPiece.getX() - newPiece.getX());
    if ((killMove && difference > 2) || difference > 1) {
      throw new MoreThanOneTileMove();
    }
  }

  private Boolean isOneMove(List<PiecePosition> white, List<PiecePosition> black, Boolean wasKillMove)
      throws MoreThanOneMoveMade {
    Integer changeAmount = 0;
    changeAmount += isOneMoveBlack(black);
    changeAmount += isOneMoveWhite(white);

    // We have to check for two changes total
    // if number of changes exceeds those than there was more than one move
    if (!(changeAmount == 2 || (changeAmount == 3 && wasKillMove))) {
      throw new MoreThanOneMoveMade();
    }
    return true;
  }

  private Integer isOneMoveBlack(List<PiecePosition> black) {
    Integer positionIndexNow;
    Integer positionIndexBefore;
    Boolean found = false;
    Integer result = 0;
    for (positionIndexBefore = 0; positionIndexBefore < this.black.size(); ++positionIndexBefore) {
      for (positionIndexNow = 0; positionIndexNow < black.size(); ++positionIndexNow) {
        if (this.black.get(positionIndexBefore).getX().equals(black.get(positionIndexNow).getX())
            && this.black.get(positionIndexBefore).getY().equals(black.get(positionIndexNow).getY())) {
          found = true;
          break;
        }
      }
      if (Boolean.TRUE.equals(found)) {
        found = false;
      } else {
        ++result;
        break;
      }
    }
    return result;
  }

  private Integer isOneMoveWhite(List<PiecePosition> white) {
    Integer positionIndexNow;
    Integer positionIndexBefore;
    Boolean found = false;
    Integer result = 0;
    for (positionIndexBefore = 0; positionIndexBefore < this.white.size(); ++positionIndexBefore) {
      for (positionIndexNow = 0; positionIndexNow < white.size(); ++positionIndexNow) {
        if (this.white.get(positionIndexBefore).getX().equals(white.get(positionIndexNow).getX())
            && this.white.get(positionIndexBefore).getY().equals(white.get(positionIndexNow).getY())) {
          found = true;
          break;
        }
      }
      if (Boolean.TRUE.equals(found)) {
        found = false;
      } else {
        ++result;
        break;
      }
    }
    return result;
  }

  private void isOverlappingMove(List<PiecePosition> white, List<PiecePosition> black) throws OverlappingPieces {
    if (Boolean.TRUE.equals(isWhiteMove(white))) {
      isOverlappingMoveWhite(white);
    } else {
      isOverlappingMoveBlack(black);
    }
  }

  private void isOverlappingMoveWhite(List<PiecePosition> white) throws OverlappingPieces {
    for (int i = 0; i < white.size(); ++i) {
      for (int j = 0; j < this.black.size(); ++j) {
        if (white.get(i).getX().equals(this.black.get(j).getX())
            && white.get(i).getY().equals(this.black.get(j).getY())) {
          throw new OverlappingPieces();
        }
      }
      for (int k = 0; k < this.white.size(); ++k) {
        if (white.get(i).getX().equals(this.white.get(k).getX())
            && white.get(i).getY().equals(this.white.get(k).getY())) {
          throw new OverlappingPieces();
        }
      }
    }
  }

  private void isOverlappingMoveBlack(List<PiecePosition> black) throws OverlappingPieces {
    for (int i = 0; i < black.size(); ++i) {
      for (int j = 0; j < this.black.size(); ++j) {
        if (black.get(i).getX().equals(this.black.get(j).getX())
            && black.get(i).getY().equals(this.black.get(j).getY())) {
          throw new OverlappingPieces();
        }
      }
      for (int k = 0; k < this.black.size(); ++k) {
        if (black.get(i).getX().equals(this.white.get(k).getX())
            && black.get(i).getY().equals(this.white.get(k).getY())) {
          throw new OverlappingPieces();
        }
      }
    }
  }

  /*
   * Changes board to contain KILLED_<COLOR> in the board. If kill move was
   * avaliable and not taken throws MandatoryKillMove. Checks whether kill move
   * was possible or not.
   *
   * @throws MandatoryKillMove
   */
  private Boolean wasKillMove() {
    for (int i = 0; i < this.board.size(); ++i) {
      for (int j = 0; j < this.board.get(i).size(); ++j) {
        if (this.board.get(i).get(j) == Piece.KILLED_BLACK || this.board.get(i).get(j) == Piece.KILLED_WHITE) {
          return true;
        }
      }
    }
    return false;
  }

  private Boolean isKillMoveAvaliable(Boolean whiteMove) {
    // We nned to have bordering pawns and with kings to check whole diagonals
    // Check for Kings
    if (Boolean.TRUE.equals(whiteMove)) {
      if (Boolean.FALSE.equals(isKillMoveAvaliableWhiteKings())
          && Boolean.FALSE.equals(isKillMoveAvaliableWhitePawns())) {
        return false;
      }
    } else {
      if (Boolean.FALSE.equals(isKillMoveAvaliableBlackKings())
          && Boolean.FALSE.equals(isKillMoveAvaliableBlackPawns())) {
        return false;
      }
    }
    return true;
  }

  private Boolean isKillMoveAvaliableWhiteKings() {
    for (int i = 0; i < this.white.size(); ++i) {
      if (this.white.get(i).getAffiliation() == Piece.WHITE_KING) {
        for (int offset = 0; offset < this.board.size(); ++offset) {
          if (Boolean.TRUE.equals(isKillMoveAvaliableLeftUpWhite(i, offset))
              || Boolean.TRUE.equals(isKillMoveAvaliableRightUpWhite(i, offset))
              || Boolean.TRUE.equals(isKillMoveAvaliableRightDownWhite(i, offset))
              || Boolean.TRUE.equals(isKillMoveAvaliableLeftDownWhite(i, offset))) {
            return true;
          }
        }
      }
    }
    return false;
  }

  private Boolean isKillMoveAvaliableWhitePawns() {
    int offset = 1;
    for (int i = 0; i < this.white.size(); ++i) {
      if (this.white.get(i).getAffiliation() == Piece.WHITE_PAWN
          && (Boolean.TRUE.equals(isKillMoveAvaliableLeftUpWhite(i, offset))
              || Boolean.TRUE.equals(isKillMoveAvaliableRightUpWhite(i, offset)))) {
        return true;
      }
    }
    return false;
  }

  private Boolean isKillMoveAvaliableBlackKings() {
    for (int i = 0; i < this.black.size(); ++i) {
      if (this.black.get(i).getAffiliation() == Piece.BLACK_KING) {
        for (int offset = 0; offset < this.board.size(); ++offset) {
          if (Boolean.TRUE.equals(isKillMoveAvaliableLeftUpBlack(i, offset))
              || Boolean.TRUE.equals(isKillMoveAvaliableRightUpBlack(i, offset))
              || Boolean.TRUE.equals(isKillMoveAvaliableRightDownBlack(i, offset))
              || Boolean.TRUE.equals(isKillMoveAvaliableLeftDownBlack(i, offset))) {
            return true;
          }
        }
      }
    }
    return false;
  }

  private Boolean isKillMoveAvaliableBlackPawns() {
    int offset = 1;
    for (int i = 0; i < this.black.size(); ++i) {
      if (this.black.get(i).getAffiliation() == Piece.BLACK_KING
          && (Boolean.TRUE.equals(isKillMoveAvaliableRightDownBlack(i, offset))
              || Boolean.TRUE.equals(isKillMoveAvaliableLeftDownBlack(i, offset)))) {
        return true;
      }
    }
    return false;
  }

  private Boolean isKillMoveAvaliableLeftUpWhite(Integer index, Integer offset) {
    try {
      Piece temp = getPieceLeftUpOffset(this.board, this.white.get(index).getX(), this.white.get(index).getY(), offset);
      if (temp.equals(Piece.BLACK_KING) || temp.equals(Piece.BLACK_PAWN)) {
        temp = getPieceLeftUpOffset(this.board, this.white.get(index).getX(), this.white.get(index).getY(), offset + 1);
        if (temp.equals(Piece.EMPTY)) {
          return true;
        }
      } else if (temp.equals(Piece.WHITE_KING) || temp.equals(Piece.WHITE_PAWN)) {
        return false;
      }
    } catch (IndexOutOfBoundsException e) {
      return false;
    }
    return false;
  }

  private Boolean isKillMoveAvaliableRightUpWhite(Integer index, Integer offset) {
    try {
      Piece temp = getPieceRightUpOffset(this.board, this.white.get(index).getX(), this.white.get(index).getY(),
          offset);
      if (temp.equals(Piece.BLACK_KING) || temp.equals(Piece.BLACK_PAWN)) {
        temp = getPieceRightUpOffset(this.board, this.white.get(index).getX(), this.white.get(index).getY(),
            offset + 1);
        if (temp.equals(Piece.EMPTY)) {
          return true;
        }
      } else if (temp.equals(Piece.WHITE_KING) || temp.equals(Piece.WHITE_PAWN)) {
        return false;
      }
    } catch (IndexOutOfBoundsException e) {
      return false;
    }
    return false;
  }

  private Boolean isKillMoveAvaliableRightDownWhite(Integer index, Integer offset) {
    try {
      Piece temp = getPieceRightDownOffset(this.board, this.white.get(index).getX(), this.white.get(index).getY(),
          offset);
      if (temp.equals(Piece.BLACK_KING) || temp.equals(Piece.BLACK_PAWN)) {
        temp = getPieceRightDownOffset(this.board, this.white.get(index).getX(), this.white.get(index).getY(),
            offset + 1);
        if (temp.equals(Piece.EMPTY)) {
          return true;
        }
      } else if (temp.equals(Piece.WHITE_KING) || temp.equals(Piece.WHITE_PAWN)) {
        return false;
      }
    } catch (IndexOutOfBoundsException e) {
      return false;
    }
    return false;
  }

  private Boolean isKillMoveAvaliableLeftDownWhite(Integer index, Integer offset) {
    try {
      Piece temp = getPieceLeftDownOffset(this.board, this.white.get(index).getX(), this.white.get(index).getY(),
          offset);
      if (temp.equals(Piece.BLACK_KING) || temp.equals(Piece.BLACK_PAWN)) {
        temp = getPieceLeftDownOffset(this.board, this.white.get(index).getX(), this.white.get(index).getY(),
            offset + 1);
        if (temp.equals(Piece.EMPTY)) {
          return true;
        }
      } else if (temp.equals(Piece.WHITE_KING) || temp.equals(Piece.WHITE_PAWN)) {
        return false;
      }
    } catch (IndexOutOfBoundsException e) {
      return false;
    }
    return false;
  }

  private Boolean isKillMoveAvaliableLeftUpBlack(Integer index, Integer offset) {
    try {
      Piece temp = getPieceLeftUpOffset(this.board, this.black.get(index).getX(), this.black.get(index).getY(), offset);
      if (temp.equals(Piece.WHITE_KING) || temp.equals(Piece.WHITE_PAWN)) {
        temp = getPieceLeftUpOffset(this.board, this.black.get(index).getX(), this.black.get(index).getY(), offset + 1);
        if (temp.equals(Piece.EMPTY)) {
          return true;
        }
      } else if (temp.equals(Piece.BLACK_KING) || temp.equals(Piece.BLACK_PAWN)) {
        return false;
      }
    } catch (IndexOutOfBoundsException e) {
      return false;
    }
    return false;
  }

  private Boolean isKillMoveAvaliableRightUpBlack(Integer index, Integer offset) {
    try {
      Piece temp = getPieceRightUpOffset(this.board, this.black.get(index).getX(), this.black.get(index).getY(),
          offset);
      if (temp.equals(Piece.WHITE_KING) || temp.equals(Piece.WHITE_PAWN)) {
        temp = getPieceRightUpOffset(this.board, this.black.get(index).getX(), this.black.get(index).getY(),
            offset + 1);
        if (temp.equals(Piece.EMPTY)) {
          return true;
        }
      } else if (temp.equals(Piece.BLACK_KING) || temp.equals(Piece.BLACK_PAWN)) {
        return false;
      }
    } catch (IndexOutOfBoundsException e) {
      return false;
    }
    return false;
  }

  private Boolean isKillMoveAvaliableRightDownBlack(Integer index, Integer offset) {
    try {
      Piece temp = getPieceRightDownOffset(this.board, this.black.get(index).getX(), this.black.get(index).getY(),
          offset);
      if (temp.equals(Piece.WHITE_KING) || temp.equals(Piece.WHITE_PAWN)) {
        temp = getPieceRightDownOffset(this.board, this.black.get(index).getX(), this.black.get(index).getY(),
            offset + 1);
        if (temp.equals(Piece.EMPTY)) {
          return true;
        }
      } else if (temp.equals(Piece.BLACK_KING) || temp.equals(Piece.BLACK_PAWN)) {
        return false;
      }
    } catch (IndexOutOfBoundsException e) {
      return false;
    }
    return false;
  }

  private Boolean isKillMoveAvaliableLeftDownBlack(Integer index, Integer offset) {
    try {
      Piece temp = getPieceLeftDownOffset(this.board, this.black.get(index).getX(), this.black.get(index).getY(),
          offset);
      if (temp.equals(Piece.WHITE_KING) || temp.equals(Piece.WHITE_PAWN)) {
        temp = getPieceLeftDownOffset(this.board, this.black.get(index).getX(), this.black.get(index).getY(),
            offset + 1);
        if (temp.equals(Piece.EMPTY)) {
          return true;
        }
      } else if (temp.equals(Piece.BLACK_KING) || temp.equals(Piece.BLACK_PAWN)) {
        return false;
      }
    } catch (IndexOutOfBoundsException e) {
      return false;
    }
    return false;
  }

  private Piece getPieceLeftUpOffset(List<List<Piece>> board, Integer x, Integer y, Integer offset)
      throws IndexOutOfBoundsException {
    if (x - offset >= 0 && y - offset >= 0) {
      return board.get(x).get(y);
    } else {
      throw new IndexOutOfBoundsException();
    }
  }

  private Piece getPieceRightUpOffset(List<List<Piece>> board, Integer x, Integer y, Integer offset)
      throws IndexOutOfBoundsException {
    if (x + offset < board.size() && y - offset >= 0) {
      return board.get(x).get(y);
    } else {
      throw new IndexOutOfBoundsException();
    }
  }

  private Piece getPieceRightDownOffset(List<List<Piece>> board, Integer x, Integer y, Integer offset)
      throws IndexOutOfBoundsException {
    if (x + offset < board.size() && y + offset < board.size()) {
      return board.get(x).get(y);
    } else {
      throw new IndexOutOfBoundsException();
    }
  }

  private Piece getPieceLeftDownOffset(List<List<Piece>> board, Integer x, Integer y, Integer offset)
      throws IndexOutOfBoundsException {
    if (x + offset >= 0 && y + offset < board.size()) {
      return board.get(x).get(y);
    } else {
      throw new IndexOutOfBoundsException();
    }
  }

  private Boolean isWhiteMove(List<PiecePosition> white) {
    Boolean found = false;
    for (int i = 0; i < this.white.size(); ++i) {
      for (int j = 0; j < white.size(); ++j) {
        if (this.white.get(i).getX().equals(white.get(j).getX())
            && this.white.get(i).getY().equals(white.get(j).getX())) {
          found = true;
          break;
        }
      }
      if (Boolean.TRUE.equals(found)) {
        found = false;
      } else {
        return true;
      }
    }
    return false;
  }

  public String toString() {
    StringBuilder result = new StringBuilder();
    for (int i = 0; i < this.board.size(); ++i) {
      for (int j = 0; j < this.board.get(i).size(); ++j) {
        switch (this.board.get(i).get(j)) {
        case EMPTY:
          result.append("0");
          break;
        case WHITE_KING:
          result.append("I");
          break;
        case WHITE_PAWN:
          result.append("W");
          break;
        case BLACK_PAWN:
          result.append("B");
          break;
        case BLACK_KING:
          result.append("X");
          break;
        case KILLED_BLACK:
          result.append("D");
          break;
        case KILLED_WHITE:
          result.append("S");
          break;
        }
      }
      result.append("\n");
    }
    return result.toString();
  }

  private List<PiecePosition> findAllWhite(List<List<Piece>> board) {
    List<PiecePosition> whiteLocal = new ArrayList<>();
    for (int i = 0; i < board.size(); ++i) {
      for (int j = 0; j < board.get(i).size(); ++j) {
        if (board.get(i).get(j).equals(Piece.WHITE_KING) || board.get(i).get(j).equals(Piece.WHITE_PAWN)) {
          whiteLocal.add(new PiecePosition(Integer.valueOf(i), Integer.valueOf(j), board.get(i).get(j)));
        }
      }
    }
    return whiteLocal;
  }

  private List<PiecePosition> findAllBlack(List<List<Piece>> board) {
    ArrayList<PiecePosition> blackLocal = new ArrayList<>();
    for (int i = 0; i < board.size(); ++i) {
      for (int j = 0; j < board.get(i).size(); ++j) {
        if (board.get(i).get(j).equals(Piece.BLACK_KING) || board.get(i).get(j).equals(Piece.BLACK_PAWN)) {
          blackLocal.add(new PiecePosition(Integer.valueOf(i), Integer.valueOf(j), board.get(i).get(j)));
        }
      }
    }
    return blackLocal;
  }

  private void isNonDiagonalMove(List<PiecePosition> white, List<PiecePosition> black) throws VerticalOrHorizontalMove {

    for (int i = 0; i < white.size(); ++i) {
      if (white.get(i).getX() % 2 == 1 && white.get(i).getY() % 2 == 0
          || white.get(i).getX() % 2 == 0 && white.get(i).getY() % 2 == 1) {
        throw new VerticalOrHorizontalMove("White made illegal move\n");
      }
    }

    for (int i = 0; i < black.size(); ++i) {
      if (black.get(i).getX() % 2 == 1 && black.get(i).getY() % 2 == 0
          || black.get(i).getX() % 2 == 0 && black.get(i).getY() % 2 == 1) {
        throw new VerticalOrHorizontalMove("Black made illegal move\n");
      }
    }
  }

  private class PiecePosition {
    public PiecePosition(Integer x, Integer y, Piece affiliation) {
      this.x = x;
      this.y = y;
      this.affiliation = affiliation;
    }

    public Integer getX() {
      return x;
    }

    public Integer getY() {
      return y;
    }

    public Piece getAffiliation() {
      return affiliation;
    }

    public void setX(Integer x) {
      this.x = x;
    }

    public void setY(Integer y) {
      this.y = y;
    }

    public void setAffiliation(Piece affiliation) {
      this.affiliation = affiliation;
    }

    private Integer x;
    private Integer y;
    private Piece affiliation;
  }

  private List<PiecePosition> white;
  private List<PiecePosition> black;
  private List<PiecePosition> whiteOld;
  private List<PiecePosition> blackOld;
  private List<List<Piece>> board;
  private List<List<Piece>> boardOld;
}