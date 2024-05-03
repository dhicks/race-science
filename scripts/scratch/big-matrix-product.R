library(tictoc)

n_docs = 26000
n_topics = 20

theta = igraph::sample_dirichlet(n_docs, rep(1/n_topics, n_topics)) |> 
    t()

## ~12 sec, result is ~5.4 GB
{
    tic()
    product = theta %*% t(theta)
    toc()
}
object.size(product)