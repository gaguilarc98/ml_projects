library(ReDaMoR)
m <- model_relational_data()
plot(m)
ReDaMoR::write_json_data_model(m, "D:/!bso/CIC/CIC_model.json")
n <- ReDaMoR::read_json_data_model("D:/!bso/CIC/CIC_model.json")
plot(n)
n
model_relational_data(n)
