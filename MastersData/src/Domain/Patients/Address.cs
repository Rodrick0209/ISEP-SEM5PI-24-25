using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Patients
{
    public class Address : IValueObject
    {
        public Street Street { get; private set; }
        public PostalCode PostalCode { get; private set; }
        public City City { get; private set; }
        public Country Country { get; private set; }

        private Address() { }

        public Address(string street, string postalCode, string city, string country)
        {
            this.Street = new Street(street);
            this.PostalCode = new PostalCode(postalCode);
            this.City = new City(city);
            this.Country = new Country(country);
        }
    }

    public class Street : IValueObject
    {
        public string street { get; private set; }

        public Street(string street)
        {
            validateStreet(street);
            this.street = street;
        }

        private void validateStreet(string street)
        {
            if (string.IsNullOrWhiteSpace(street))
            {
                throw new System.ArgumentException("Street must not be empty.");
            }
        }
    }

    public class PostalCode : IValueObject
    {
        public string postalCode { get; private set; }

        public PostalCode(string postalCode)
        {
            validatePostalCode(postalCode);
            this.postalCode = postalCode;
        }

        private void validatePostalCode(string postalCode)
        {
            if (string.IsNullOrWhiteSpace(postalCode))
            {
                throw new System.ArgumentException("Postal code must not be empty.");
            }
        }
    }

    public class City : IValueObject
    {
        public string city { get; private set; }

        public City(string city)
        {
            validateCity(city);
            this.city = city;
        }

        private void validateCity(string city)
        {
            if (string.IsNullOrWhiteSpace(city))
            {
                throw new System.ArgumentException("City must not be empty.");
            }
        }
    }

    public class Country : IValueObject
    {
        public string country { get; private set; }

        public Country(string country)
        {
            validateCountry(country);
            this.country = country;
        }

        private void validateCountry(string country)
        {
            if (string.IsNullOrWhiteSpace(country))
            {
                throw new System.ArgumentException("Country must not be empty.");
            }
        }
    }
}