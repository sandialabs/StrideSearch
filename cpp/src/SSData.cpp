#include "SSData.hpp"

namespace StrideSearch {

SSData::SSData(std::shared_ptr<NCReader> rdr) : reader(rdr), points(rdr->makePoints()), adaptor(points),
    tree(3,adaptor,nf::KDTreeSingleIndexAdaptorParams(max_leaf)) {}

}
