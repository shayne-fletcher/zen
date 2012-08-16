#include <io.h>

#include <vector>
#include <string>
#include <iostream>
#include <cassert>

std::vector<std::string>
harvest(std::string const& expr)
{
  std::vector<std::string> paths;
  std::string const selfdir=".",parentdir="..";

  struct _finddata_t fileinfo;
  intptr_t group = _findfirst(expr.c_str(), &fileinfo);
  if(group == -1)
    {
      errno_t err;
      _get_errno(&err);
      if(err==ENOENT || err==EINVAL)
	return paths;
      assert(false);
    }

  int val;

 harvestnext:
  if(fileinfo.name != parentdir && fileinfo.name != selfdir)
    paths.push_back(fileinfo.name);
  val = _findnext(group, &fileinfo);
  if(val == 0)
    {
      goto harvestnext;
    }
  else if(val == -1)
    {
      errno_t err;
      _get_errno(&err);
      if(errno == ENOENT)
	goto harvestexit;
      assert(false);
    }
  else assert(false); 

harvestexit:
  _findclose(group);

  return paths;
}

int main(int argc, char const* argv[])
{
  std::string expr;

  if(argc == 1)
    {
      std::cerr << "usage : " << argv[0] << " expr\n";
      
      return 1;
    }
  if(argc == 2)
    {
      expr = argv[1];
    }
    
  std::vector<std::string> paths =  harvest(expr);

  std::cout << "Matches:\n";
  for(std::size_t i = 0; i < paths.size(); ++i)
    {
      std::cout << paths[i] << std::endl;
    }

  return 0;
}

