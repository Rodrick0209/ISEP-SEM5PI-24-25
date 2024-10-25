using System.Diagnostics;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.User;
using DDDSample1.Infrastructure;

public static class Utilities
{
    public static void InitializeDbForTests(DDDSample1DbContext db)
    {
        db.Patients.AddRange(GetSeedingPatients());
        db.Users.AddRange(GetSeedingUsers());
            
        db.SaveChanges();
    }

    public static void ReinitializeDbForTests(DDDSample1DbContext db)
    {
        db.Patients.RemoveRange(db.Patients);
        db.Users.RemoveRange(db.Users);
        
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

    public static List<User> GetSeedingUsers()
    {
        PasswordHasher passwordHasher = new PasswordHasher();
        return new List<User>()
        {
            new User("D202512345@gmail.com", "Doctor", passwordHasher.HashPassword("password")),
            new User("D202512344@gmail.com", "Doctor", passwordHasher.HashPassword("password")),
            new User("admin@teste.com", "admin", passwordHasher.HashPassword("password")),
            new User("jane.doe@gmail.com", "patient", passwordHasher.HashPassword("password")),
        };
    }
}