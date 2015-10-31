package api

type Response struct {
	ID   int           // The original caller ID
	Data []interface{} // The data returned from the call
}
