using System.Diagnostics;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.Patients;
using DDDSample1.Infrastructure;

public static class Utilities
{
    public static void InitializeDbForTests(DDDSample1DbContext db)
    {
        db.Patients.AddRange(GetSeedingPatients());
        db.SaveChanges();
    }

    public static void ReinitializeDbForTests(DDDSample1DbContext db)
    {
        db.Patients.RemoveRange(db.Patients);
        InitializeDbForTests(db);
    }

    



    public static List<Patient> GetSeedingPatients()
    {
        return new List<Patient>()
        {
            new Patient(
                "John Doe",
                "1990-01-01",
                "male", 
                "john.doe@gmail.com", 
                "+351 123456789", 
                "address1", 
                "1234-123",
                "city", 
                "country", 
                "Jane Doe",
                "jane.doe@gmail.com", 
                "+351 123456789", 
                "202410000001"
                ),
            new Patient(
                "Jane Doe", 
                "1990-01-01",
                "female", 
                "jane.doe@gmail.com", 
                "+351 234123541", 
                "address2", 
                "1234-123", 
                "city", 
                "country", 
                "John Doe",
                "john.doe@gmail.com",
                "+351 123456789",
                "202410000002"
                )
        }; 
    }
}