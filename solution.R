book.total_volumes <- function(book) {
  df1 = book[[1]]
  df2 = book[[2]]
  val1 = df1['size']
  val2 = df2['size']
  total_volumes1 = as.data.frame(colSums(val1))
  total_volumes2 = as.data.frame(colSums(val2))
  total_volumes = c(ask = total_volumes1, bid = total_volumes2)

  return(total_volumes)
  
  # Arguments:
  #   book - A list containing "ask" and "bid", each of which are dataframes
  #       containing the collection of limit orders.
  # book value is a 2 dataframes stored in a list
  # Returns:
  #   The total volume in the book.
}

book.best_prices <- function(book) {
  df1 = book[[1]]
  df2 = book[[2]]
  price1 = min(df1['price'])
  price2 = max(df2['price'])
  best_prices = c(ask = price1, bid = price2)
  
  return(best_prices)
  
  
  
  # Arguments:
  #   book - A list containing "ask" and "bid", each of which are dataframes
  #       containing the collection of limit orders.
  #
  # Returns:
  #   A list with "ask" and "bid", the values of which are the best prices in
  #       the book.
}

book.midprice <- function(book) {
  df1 = book[[1]]
  df2 = book[[2]]
  price1 = min(df1['price'])
  price2 = max(df2['price'])
  total = price1 + price2
  midprice = total/2
  return(midprice)
  # Arguments:
  #   book - A list containing "ask" and "bid", each of which are dataframes
  #       containing the collection of limit orders.
  #
  # Returns:
  #   The midprice of the book.
  
}

book.spread <- function(book) {
  df1 = book[[1]]
  df2 = book[[2]]
  val1 = min(df1['price'])
  val2 = max(df2['price'])
  price1 = as.numeric(val1)
  price2 = as.numeric(val2)
  spread = price1-price2
  return(spread)
  # Arguments:
  #   book - A list containing "ask" and "bid", each of which are dataframes
  #       containing the collection of limit orders.
  #
  # Returns:
  #   The spread of the book.
}

book.add <- function(book, message) {
    
  
  df1 = book[[1]]
  df2 = book[[2]]
  oid = message$oid
  side = message$side
  price = message$price
  size = message$size
  
  if (side == 'S') {
    # ask
    if (price > max(df2['price'])) {
      newdf = data.frame(oid, price, size)
      names(newdf) = c("oid", "price", "size")
      final1 = rbind(df1, newdf)
      book = list(final1,df2)
      names(book) = c("ask", "bid")
      
    }else if (size < df2[1,'size'] & (price < max(df2$price)) ){
      newsize = df2[1,'size'] - size
      df2[1,'size'] = newsize
      
    }else if (size == df2[1,'size'] & (price <= max(df2['price'])) ){
      #remove the row !!!#####################################
      df2 = subset(df2, size != df2[1,'size'])
      
    }else if ((price <= max(df2['price'])) & (size > df2[1,'size']) ) {#price comparison 
      com = df1[df1$price <= price, ]
      names(com) = c("oid", "price", "size")
      
      #delete first row !!!!###################################
      finsize1 = size - df2[1,'size']
      df2 = subset(df2, size != df2[1,'size'])
      
      #assign rest to df1
      newdf = data.frame(oid, price, finsize1)
      names(newdf) = c("oid", "price", "size")
      df1 = rbind(df1, newdf)
    }
    
    
      
    }else if (side == 'B') {
      # bid
      if (price < min(df1['price'])) {
        newdf = data.frame(oid, price, size)
        names(newdf) = c("oid", "price", "size")
        final2 = rbind(df2, newdf)
        book = list(df1, final2)
        names(book) = c("ask", "bid")
        
      }else if (size < df1[1,'size'] & (price > min(df1['price'])) ){
        newsize = df1[1,'size'] - size
        df1[1,'size'] = newsize
        
      }else if (size == df1[1,'size'] & (price >= min(df1['price'])) ){
        #remove the row !!!!############################
        df1 = subset(df1, size != df1[1,'size'])
        
      }else if ((price >= min(df1['price'])) & (size > df1[1,'size']) ) {#price comparison 
        com = df1[df1$price <= price, ]
        names(com) = c("oid", "price", "size")
        #delete first row !!!!########################
        
        finsize2 = size - df1[1,'size']
        df1 = subset(df1, size != df1[1,'size'])
        
        print(finsize2)
        #assign rest to df2
        newdf = data.frame(oid, price, finsize2)
        names(newdf) = c("oid", "price", "size")
        df2 = rbind(df2, newdf)
      }
      
}
  
  
  
  
  # Arguments:
  #   book - A list containing "ask" and "bid", each of which are dataframes
  #       containing the collection of limit orders.
  #   message - A list containing "oid", "side", "price" and "size" entries.
  #
  # Returns:
  #   The updated book.
  
  book = list(df1,df2)
  names(book) = c("ask", "bid")
  return(book)
}

book.reduce <- function(book, message) {
  
  
  df1 = book[[1]]
  df2 = book[[2]]
  index = message$oid
  msize = message$amount
  
  row1 = df1[df1$oid == message$oid, ]
  names(row1) = c("oid", "price", "size")
  namer1 = rownames(row1)
  
  row2 = df2[df2$oid == message$oid, ]
  names(row2) = c("oid", "price", "size")
  namer2 = rownames(row2)
  
  if (nrow(row1) > 0){
    if (row1$oid == index){
      if (row1$size > msize) {
        #reduce size
        size = row1$size - msize
        df1[namer1,'size'] = size
      
    } else if (row1$size <= msize){
      #remove############################
      df1 = subset(df1, df1$oid != index)
     }
    }
    
    } else if (nrow(row2) > 0) {
      if (row2$oid == index){
      if (df2$size > msize) {
        #reduce size
        size = row2$size - msize
        df2[namer2,'size'] = size
        
      }else if (row1$size <= msize){
        #remove try another method?!
        df2 = subset(df2, df2&oid != index)
      }
      }
    }
  book = list(df1,df2)
  names(book) = c("ask", "bid")  
  return(book)
}
# Arguments:
#   book - A list containing "ask" and "bid", each of which are dataframes
#       containing the collection of limit orders.
#   message - A list containing "oid" and "amount".
#
# Returns:
#   The updated book.
###############################################################################
###############################################################################

