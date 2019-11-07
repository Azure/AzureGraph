# get certificate details to add to an app
read_cert <- function(cert)
{
    UseMethod("read_cert")
}


read_cert.character <- function(cert)
{
    if(file.exists(cert))
        read_cert_file(cert)
    else paste0(cert, collapse="")
}


read_cert.raw <- function(cert)
{
    openssl::base64_encode(cert)
}


read_cert.rawConnection <- function(cert)
{
    openssl::base64_encode(rawConnectionValue(cert))
}


# AzureKeyVault::stored_cert
read_cert.stored_cert <- function(cert)
{
    cert$cer
}


# openssl::cert
read_cert.cert <- function(cert)
{
    openssl::base64_encode(cert)
}


read_cert_file <- function(file)
{
    ext <- tolower(tools::file_ext(file))
    if(ext == "pem")
    {
        pem <- openssl::read_cert(file)
        openssl::base64_encode(pem)
    }
    else if(ext %in% c("p12", "pfx"))
    {
        pfx <- openssl::read_p12(file)
        pfx$cert
    }
    else stop("Unsupported file extension: ", ext, call.=FALSE)
}

