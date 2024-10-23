using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
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
  public static async Task SeedAsync(IServiceProvider serviceProvider)
  {
    using var scope = serviceProvider.CreateScope();
    var context = scope.ServiceProvider.GetRequiredService<DDDSample1DbContext>();

    // SeedUsers(context, new User("admin@teste.com", "admin"), "password");

    Patient lastPatientInMonth = context.Patients
                .Where(p => p.MedicalRecordNumber._medicalRecordNumber.Substring(0,6) == DateTime.Now.ToString("yyyyMM"))
                .OrderByDescending(p => p.MedicalRecordNumber._medicalRecordNumber)
                .FirstOrDefault();

    Patient johnCena = new Patient(
      "John Cena",
      "2022-10-01",
      "male",
      "john.cena@example.com",
      "123456123",
      "Jane Cena",
      "jane.cena@example.com",
      "234567234",
      MedicalRecordNumberGenerator.GenerateMedicalRecordNumber(lastPatientInMonth)
    );

    SeedPatients(context, johnCena);

    Patient johnCena2 = new Patient(
      "John Cena2",
      "2022-10-01",
      "male",
      "john.cena@example.com",
      "123456123",
      "945123111",
      MedicalRecordNumberGenerator.GenerateMedicalRecordNumber());
    SeedPatients(context, johnCena2);


    Patient johnCena3 = new Patient(
      "John Cena3",
      "2022-10-01",
      "male",
      "john.cena@example.com",
      "123456123",
      "945123111",
      MedicalRecordNumberGenerator.GenerateMedicalRecordNumber());
    SeedPatients(context, johnCena3);


    Patient johnCena4 = new Patient(
      "John Cena",
      "2022-10-01",
      "male",
      "john.cena@example.com",
      "123456123",
      "945123111",
      MedicalRecordNumberGenerator.GenerateMedicalRecordNumber());
    SeedPatients(context, johnCena4);


    Patient johnCena5 = new Patient(
      "John Cena",
      "2022-10-01",
      "male",
      "john.cena@example.com",
      "123456123",
      "945123111",
      MedicalRecordNumberGenerator.GenerateMedicalRecordNumber());
    SeedPatients(context, johnCena5);


    var specialization1 = new Specialization("Ortopedia");
    var specialization2 = new Specialization("Oncologia");
    var specialization3 = new Specialization("Obstetricia");
    SeedSpecializations(context, specialization1);
    SeedSpecializations(context, specialization2);
    SeedSpecializations(context, specialization3);


    // Create required staff
    var requiredStaff1 = new RequiredStaff(10, specialization1.Id);
    var requiredStaff2 = new RequiredStaff(20, specialization2.Id);
    var requiredStaffList1 = new List<RequiredStaff> { requiredStaff1, requiredStaff2 };
    var requiredStaffList2 = new List<RequiredStaff> { new RequiredStaff(20, specialization2.Id) }; // New instance
    var requiredStaffList3 = new List<RequiredStaff> { new RequiredStaff(2, specialization2.Id) }; // New instance

    var user = new User("D202512345@gmail.com","Doctor");
    var user2 = new User("D202512344@gmail.com","Doctor");

    // Create new phases with required staff
    var phase1 = new Phase(20, requiredStaffList1);
    var phase2 = new Phase(90, requiredStaffList2);
    var phase3 = new Phase(15, requiredStaffList3);

    // Create a new operation type with the phases and specialization
    var operationType = new OperationType("New Operation Type", true, phase1, phase2, phase3, specialization1.Id);
    var operationType2 = new OperationType("New Operation Type2", true, phase1, phase2, phase3, specialization2.Id);
    
    var operationRequest = new OperationRequest("2025-02-18","emergency",johnCena.Id.AsString(),operationType.Id.AsString(),new StaffId("D202512345").AsString(),new StaffId("D202512344").AsString());
    var operationRequest2 = new OperationRequest("2025-02-18","emergency",johnCena2.Id.AsString(),operationType2.Id.AsString(),new StaffId("D202512344").AsString(),new StaffId("D202512345").AsString());
    var operationRequest3 = new OperationRequest("2025-02-18","emergency",johnCena2.Id.AsString(),operationType2.Id.AsString(),new StaffId("D202512344").AsString(),new StaffId("D202512345").AsString());
    var operationRequest4 = new OperationRequest("2025-02-18","emergency",johnCena3.Id.AsString(),operationType2.Id.AsString(),new StaffId("D202512344").AsString(),new StaffId("D202512345").AsString());
    var operationRequest5 = new OperationRequest("2025-02-18","emergency",johnCena4.Id.AsString(),operationType2.Id.AsString(),new StaffId("D202512344").AsString(),new StaffId("D202512345").AsString());
    var operationRequest6 = new OperationRequest("2025-02-18","emergency",johnCena5.Id.AsString(),operationType2.Id.AsString(),new StaffId("D202512344").AsString(),new StaffId("D202512345").AsString());


    // Seed the operation type into the context
    SeedOperationType(context, operationType);
    SeedOperationType(context, operationType2);
    SeedOperationRequest(context, operationRequest);
    SeedOperationRequest(context,operationRequest2);
    SeedOperationRequest(context,operationRequest3);
    SeedOperationRequest(context,operationRequest4);
    SeedOperationRequest(context,operationRequest5);
    SeedOperationRequest(context,operationRequest6);
    SeedUsers(context,user,"password");
    SeedUsers(context,user2,"password");

    context.SaveChanges();
  }

  private static void SeedOperationRequest(DDDSample1DbContext context, OperationRequest operationRequest)
  {
    if (!context.OperationRequests.Any())
    {
      context.OperationRequests.Add(operationRequest);
    }
  }


  private static void SeedUsers(DDDSample1DbContext context, User user, string pass)
  {
    PasswordHasher passwordHasher = new PasswordHasher();
    string password = passwordHasher.HashPassword(pass);
    user.SetPassword(password);
    if (!context.Users.Any())
    {
      context.Users.Add(user);
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
