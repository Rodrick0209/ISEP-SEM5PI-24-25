using System;
using System.Collections.Generic;
using System.Linq;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.Families;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.User;
using DDDSample1.Domain.Utils;
using DDDSample1.Infrastructure;
using Microsoft.Extensions.DependencyInjection;

public static class DataSeeder
{
  public static void Seed(IServiceProvider serviceProvider)
  {
    using var scope = serviceProvider.CreateScope();
    var context = scope.ServiceProvider.GetRequiredService<DDDSample1DbContext>();


    // Example: Add Families if none exist
    if (!context.Families.Any())
    {
      context.Families.AddRange(
          new Family("1", "Family1"),
          new Family("2", "Family2")
      );
    }

   // SeedUsers(context, new User("admin@teste.com", "admin"), "password");

    SeedPatients(context, new Patient(
      "John Cena",
      "2022-10-01",
      "male",
      "john.cena@example.com",
      "123456123",
      "945123111",
      MedicalRecordNumberGenerator.GenerateMedicalRecordNumber()
    ));

    var specialization = new Specialization("Ortopedia");

    // Create required staff
    var requiredStaff1 = new RequiredStaff(1, specialization.Id);
    var requiredStaff2 = new RequiredStaff(2, specialization.Id);
    var requiredStaff3 = new RequiredStaff(3, specialization.Id);
    var requiredStaffList = new List<RequiredStaff> { requiredStaff1, requiredStaff2 };
    var requiredStaffList1 = new List<RequiredStaff> { requiredStaff3};
    
    
    
    
    //SeedOperationType(context,new OperationType("1","New Operation Type",true,new Phase("1"),new Phase("2"),new Phase("3"),new Specialization("Ortopedia")));
    

    SeedSpecializations(context,specialization);


    // Create new phases with required staff
    var phase1 = new Phase( 30, requiredStaffList1);
    var phase2 = new Phase(45, requiredStaffList);
    var phase3 = new Phase(60, requiredStaffList);

    // Create a new operation type with the phases and specialization
    var operationType = new OperationType("New Operation Type", true, phase1, phase2, phase3, specialization.Id);

  


    // Seed the operation type into the context
    SeedOperationType(context, operationType);

    

    context.SaveChanges();
  }


  private static void SeedUsers(DDDSample1DbContext context, User user, string pass)
  {
    PasswordHasher passwordHasher = new PasswordHasher();
    string password = passwordHasher.HashPassword(pass);
    user.SetPassword(password);
    if (!context.Users.Any())
    {
      context.Users.AddRange(user);
    }
  }

  private static void SeedOperationType(DDDSample1DbContext context, OperationType operationType)
  {
    if (!context.OperationTypes.Any())
    {
      context.OperationTypes.Add(operationType);
    }
  }

  private static void SeedPatients(DDDSample1DbContext context, Patient patient)
  {
    if (!context.Patients.Any())
    {
      context.Patients.Add(patient);
    }
  }

  private static void SeedSpecializations(DDDSample1DbContext context, Specialization specialization)
  {
    if (!context.Specializations.Any())
    {
      context.Specializations.Add(specialization);
    }
  }

  public static void SeedStaff(DDDSample1DbContext context)
  {
      var staffMembers = new List<Staff>
      {
          new Staff(
              new StaffId("1"), 
              "John Doe", 
              "LN123456", 
              new SpecializationId(Guid.NewGuid()), 
              new AvailabilitySlotsId(Guid.NewGuid()), 
              "john.doe@example.com", 
              "123-456-7890", 
              "Doctor"
          ),
          new Staff(
              new StaffId("2"), 
              "Jane Smith", 
              "LN654321", 
              new SpecializationId(Guid.NewGuid()), 
              new AvailabilitySlotsId(Guid.NewGuid()), 
              "jane.smith@example.com", 
              "098-765-4321", 
              "Nurse"
          )
          // Adicione mais instâncias conforme necessário
      };

      context.Set<Staff>().AddRange(staffMembers);
      context.SaveChanges();
  }

}
