/////////////////////////////////// Q1 ////////////////////////////////
function all(promises) {
  return new Promise( (resolve, reject) => {
 
   new Promise((resolve, reject) => {   
   
   let valuesResultArray=[];
   let counter=0;
   for (i=0; i<promises.length;i++)
	{
		let curr=i;
		promises[i].then(content => {
		counter++;
		valuesResultArray[curr]= content;
		 
		if (counter==promises.length)
		   resolve(valuesResultArray); 
		}
		)
		.catch(err => {reject(err)
					   i=promises.length});
	   }
	   
	
   }).then((content)=>{
	   resolve(content);})
   .catch(err=>reject(err));
  });
}


function* filterGen(generator, filterFunc) {
    for (let x of generator) {
        if (filterFunc(x)) {
            yield x;
        }
    }
}
  
///////////////////////////////////////// Q2 /////////////////////////////////////////  
function* sieve(generator) {
	let prevNumbers=[];
	
    for (let y of filterGen(generator, x => {

		for (let i=0; i< prevNumbers.length; i++)
		{
		  if ((x %prevNumbers[i])==0)
		  {
			  return false;
		  }
		}
		
		prevNumbers.push(x);
		return true; }))
	{
		yield y;
	}
}