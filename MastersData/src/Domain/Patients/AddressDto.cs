namespace DDDSample1.Domain.Patients
{
    public class AddressDto
    {
        public string Street { get; set; }
        public string PostalCode { get; set; }
        public string City { get; set; }
        public string Country { get; set; }

        public AddressDto(string street, string postalCode, string city, string country)
        {
            this.Street = street;
            this.PostalCode = postalCode;
            this.City = city;
            this.Country = country;
        }
    }
}