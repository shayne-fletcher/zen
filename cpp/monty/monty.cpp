#include <tuple>
#include <cstdlib>
#include <string>
#include <exception>
#include <iostream>

namespace monty {

  namespace detail {
    inline int rand (int d) { 
      return std::rand () % d;
    }
  }

  //`dtr (w, p, d)` where `d` is the number of doors, selects "which
  //door to remain" (closed) given the winning door `w` and the player
  //chosen door `p`
  inline int dtr (int w, int p, int d) {
    return (p != w) ? w : ((p == 0) ? (d - 1) : 0);
  }

  //`gen_game (d)` generates a game with `d` doors modeled as a
  //winning door, a player selected door and the door to keep closed
  //before asking if the player wants to switch
  std::tuple<int, int, int> gen_game (int d) {
    int w = detail::rand (d), p = detail::rand (d);
    return std::make_tuple (w, p, dtr (w, p, d));
  }

  int num_wins; //To keep track of scores
  enum struct strategy {hold, switch_}; //The type of strategies

  //Play a single game
  void play_game (int d, strategy s) {
    int w, p, r;
    std::tie (w, p, r) = gen_game (d);
    switch (s) {
    case strategy::hold: num_wins += int (p == w); break;
    case strategy::switch_: num_wins += int (r == w); break;
    }
  }

  //Play a set of games
  void play_games (int d, int n, strategy s) {
    for (int i = 1; i < n; ++i) {
      play_game (d, s);
    }
  }

}//namespace monty

//Driver
int main (int argc, char const* argv[]) {
  try {

    //Parse command line
    int d, n;
    if (argc < 3)
      throw std::invalid_argument ("Missing argument");
    std::tie (d, n) = std::make_tuple(std::stoi (argv[1]), std::stoi (argv[2]));
    if (d < 3)
      throw std::invalid_argument ("Number of doors must be >= 3");
    if (n < 1)
      throw std::invalid_argument ("Number of simulations must be >= 1");

    using namespace monty;

    double err;

    num_wins = 0;
    play_games (d, n, strategy::hold);
    err = std::abs (double (num_wins)/double (n) - 1 / double (d));
    std::cout << "Num wins (hold) : " << num_wins << std::endl;
    std::cout << "Error : " << err << std::endl;

    num_wins = 0;
    play_games (d, n, strategy::switch_);
    err = std::abs (double (num_wins)/double (n) - double (d - 1)/ double (d));
    std::cout << "Num wins (switch) : " << num_wins << std::endl;
    std::cout << "Error : " << err << std::endl;
  }
  catch (std::invalid_argument const& i) {
    std::cerr << "At least one argument was invalid : " << i.what () ;
  }

  return 0;
}
