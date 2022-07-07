# Read c++ output
out_R = Proto_in_cplusplus()

# Read proto file in R
RProtoBuf::readProtoFiles(dir = "./inst/proto")
ls("RProtoBuf:DescriptorPool")

# Read message in R
writeLines(as.character(RProtoBuf::read(MyNamespace.MySave, out_R)))

