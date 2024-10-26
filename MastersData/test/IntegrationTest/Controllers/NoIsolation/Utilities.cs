using System.Diagnostics;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.User;
using DDDSample1.Infrastructure;

public static class Utilities
{
    public static void InitializeDbForTests(DDDSample1DbContext db)
    {
        

        db.Patients.AddRange(GetSeedingPatients());
        db.Users.AddRange(GetSeedingUsers());
        db.OperationTypes.AddRange(GetOperationTypes());
        db.StaffMembers.AddRange(GetStaff());

        db.SaveChanges();
    }

    public static void ReinitializeDbForTests(DDDSample1DbContext db)
    {
        db.Patients.RemoveRange(db.Patients);
        db.Users.RemoveRange(db.Users);
        db.OperationTypes.RemoveRange(db.OperationTypes);
        db.StaffMembers.RemoveRange(db.StaffMembers);

        InitializeDbForTests(db);
    }


    public static List<Staff> GetStaff()
    {
        var specialization1 = new Specialization("Ortopedia");
        var availabilitySlot1 = new AvailabilitySlot("ola");
        var availabilitySlot2 = new AvailabilitySlot("boas");
        var availabilitySlot3 = new AvailabilitySlot("tudo");

        var availabilitySlots = new List<AvailabilitySlot> { availabilitySlot1, availabilitySlot2, availabilitySlot3 };

        return new List<Staff>() { new Staff(new StaffId("D202512349"), "Miguel", "12345", specialization1.Id.AsString(), availabilitySlot1.Id.AsString(), "mamarNoC@gmail.com", "+351123456789", "Doctor"),
            new Staff(new StaffId("D202512344"), "Miguel", "12345", specialization1.Id.AsString(), availabilitySlot1.Id.AsString(),"test3Mail@gmail.com", "+351113456789", "Doctor"),
            new Staff(new StaffId("D202512345"), "Miguel", "12345", specialization1.Id.AsString(), availabilitySlot1.Id.AsString(),"test4Mail@gmail.com", "+351133456789", "Doctor")};

    }



    public static List<OperationType> GetOperationTypes()
    {
        var specialization1 = new Specialization("Ortopedia");

        var requiredStaff1 = new RequiredStaff(10, specialization1.Id);
        var requiredStaff2 = new RequiredStaff(20, specialization1.Id);
        var requiredStaffList1 = new List<RequiredStaff> { requiredStaff1, requiredStaff2 };
        var requiredStaffList2 = new List<RequiredStaff> { new RequiredStaff(20, specialization1.Id) };
        var requiredStaffList3 = new List<RequiredStaff> { new RequiredStaff(2, specialization1.Id) };

        var phase1 = new Phase(20, requiredStaffList1);
        var phase2 = new Phase(90, requiredStaffList2);
        var phase3 = new Phase(15, requiredStaffList3);

        return new List<OperationType>()
        {
            new OperationType("New Operation Type", true, phase1, phase2, phase3, specialization1.Id)
        };

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
            new User("D202512349@gmail.com", "Doctor", passwordHasher.HashPassword("password")),
            new User("admin@teste.com", "admin", passwordHasher.HashPassword("password")),
            new User("jane.doe@gmail.com", "patient", passwordHasher.HashPassword("password")),
        };
    }
}