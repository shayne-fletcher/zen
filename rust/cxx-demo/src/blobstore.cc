#include "cxx-demo/include/blobstore.h"
#include "cxx-demo/src/main.rs.h"

#include <set>
#include <string>
#include <functional>
#include <algorithm>
#include <unordered_map>

// Toy implementation of an in-memory blobstore.
//
// In reality the implementation of a BlobstoreClient could be a large
// complex C++ library.
class BlobstoreClient::impl {
  friend BlobstoreClient;
  using Blob = struct {
    std::string data;
    std::set<std::string> tags;
  };
  std::unordered_map<uint64_t, Blob> blobs;
};

BlobstoreClient::BlobstoreClient()
  : impl_{ std::make_shared<BlobstoreClient::impl>() }
{}

std::unique_ptr<BlobstoreClient> new_blobstore_client() {
  return std::make_unique<BlobstoreClient>();
}

uint64_t BlobstoreClient::put(MultiBuf& buf) const {
  // Traverse the caller's chunk iterator.
  std::string contents;
  while(true) {
    auto chunk = next_chunk(buf);
    if (chunk.size() == 0) {
      break;
    }
    contents.append(reinterpret_cast<char const*>(chunk.data()), chunk.size());
  }

  // Pretend we did something useful to persist the data.
  auto blob_id = std::hash<std::string>{}(contents);

  return blob_id;
}

// Add tag to an existing blob.
void BlobstoreClient::tag(uint64_t blobid, rust::Str tag) const {
  impl_->blobs[blobid].tags.emplace(tag);
}

// Retrieve metadata about a blob.
BlobMetadata BlobstoreClient::metadata(uint64_t blobid) const {
  BlobMetadata metadata{};
  auto blob = impl_->blobs.find(blobid);
  if (blob != impl_->blobs.end()) {
    metadata.size = blob->second.data.size();
    std::for_each(blob->second.tags.cbegin(), blob->second.tags.cend(),
                  [&](auto &t) { metadata.tags.emplace_back(t); });
  }
  return metadata;
}
