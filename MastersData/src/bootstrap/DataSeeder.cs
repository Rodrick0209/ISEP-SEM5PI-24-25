
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Domain.AvailabilitySlots;
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

    Patient johnCena = new Patient(
      "John Cena",
      "2022-10-01",
      "male",
      "john.cena@example.com",
      "+351 123456123",
      "Main Street 123",
      "1234-567",
      "Los Angeles",
      "USA",
      "Jane Cena",
      "jane.cena@example.com",
      "+351 234567234",
      "202410000001");

    Patient johnCena2 = new Patient(
      "John Cena2",
      "2022-10-01",
      "male",
      "john.cena2@example.com",
      "+351 123456121",
      "Main Street 123",
      "1234-567",
      "Los Angeles",
      "USA",
      "Jane Cena",
      "jane.cena@example.com",
      "+351 234567234",
      "202410000002");

    Patient johnCena3 = new Patient(
      "John Cena3",
      "2022-10-01",
      "male",
      "john.cena3@example.com",
      "+351 123456621",
      "Main Street 123",
      "1234-567",
      "Los Angeles",
      "USA",
      "Jane Cena",
      "jane.cena@example.com",
      "+351 234567234",
      "202410000003");

    Patient johnCena4 = new Patient(
      "John Cena",
      "2022-10-01",
      "male",
      "john.cena4@example.com",
      "+351 123412621",
      "Main Street 123",
      "1234-567",
      "Los Angeles",
      "USA",
      "Jane Cena",
      "jane.cena@example.com",
      "+351 234567234",
      "202410000004");

    Patient johnCena5 = new Patient(
      "John Cena",
      "2022-10-01",
      "male",
      "john.cena5@example.com",
      "+351 123412421",
      "Main Street 123",
      "1234-567",
      "Los Angeles",
      "USA",
      "Jane Cena",
      "jane.cena@example.com",
      "+351 234567234",
      "202410000005");

    var patients = new List<Patient> { johnCena, johnCena2, johnCena3, johnCena4, johnCena5 };

    context.Patients.AddRange(patients);

    var specialization1 = new Specialization("Ortopedia");
    var specialization2 = new Specialization("Oncologia");
    var specialization3 = new Specialization("Obstetricia");

    var specializations = new List<Specialization> { specialization1, specialization2, specialization3 };

    context.Specializations.AddRange(specializations);

    // Exemplo: Convertendo a data para uma string válida antes de passar para AvailabilitySlot
    string date = "2025-01-01"; // ou extraído de outra fonte como DateTime.Now.ToString("yyyy-MM-dd")
    string startTime = "08:00";
    string endTime = "12:00";

    var availabilitySlot1 = new AvailabilitySlot(date, startTime, endTime);

    var availabilitySlots = new List<AvailabilitySlot> { availabilitySlot1 };



    // Create required staff
    var requiredStaff1 = new RequiredStaff(10, specialization1.Id);
    var requiredStaff2 = new RequiredStaff(20, specialization2.Id);
    var requiredStaffList1 = new List<RequiredStaff> { requiredStaff1, requiredStaff2 };
    var requiredStaffList2 = new List<RequiredStaff> { new RequiredStaff(20, specialization2.Id) }; // New instance
    var requiredStaffList3 = new List<RequiredStaff> { new RequiredStaff(2, specialization2.Id) }; // New instance

    PasswordHasher hasher = new PasswordHasher();
    string password = hasher.HashPassword("password");
    var user = new User("D202512345@gmail.com", "Doctor", password);
    var user2 = new User("D202512344@gmail.com", "Doctor", password);
    var user3 = new User("admin@teste.com", "admin", password);
    var user4 = new User("john.cena@gmail.com", "patient", password);
    var users = new List<User> { user, user2, user3 };

    context.Users.AddRange(users);

    // Create new phases with required staff
    var phase1 = new Phase(20, requiredStaffList1);
    var phase2 = new Phase(90, requiredStaffList2);
    var phase3 = new Phase(15, requiredStaffList3);
    var phases = new List<Phase> { phase1, phase2, phase3 };

    context.Phases.AddRange(phases);

    // Create a new operation type with the phases and specialization
    var operationType = new OperationType("New Operation Type", true, phase1, phase2, phase3, specialization1.Id);
    var operationType2 = new OperationType("New Operation Type2", true, phase1, phase2, phase3, specialization2.Id);
    var operationTypes = new List<OperationType> { operationType, operationType2 };

    context.OperationTypes.AddRange(operationTypes);

    var operationRequest = new OperationRequest("2025-02-18", "emergency", johnCena.Id.AsString(), operationType.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512344").AsString());
    var operationRequest2 = new OperationRequest("2025-02-18", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    var operationRequests = new List<OperationRequest> { operationRequest, operationRequest2 };

    Staff staff = new Staff(new StaffId("D202512345"), "staff", "12345", specialization1.Id.Value, availabilitySlot1.Id.Value, "email@gmail.com", "+951999999999", "Doctor", "True");
    SeedStaff(context, staff);

    context.OperationRequests.AddRange(operationRequest);
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


  public static void SeedStaff(DDDSample1DbContext context, Staff staff)

  {
    if (!context.Specializations.Any())
    {
      context.StaffMembers.Add(staff);
    }
  }



  private static void SeedAvailabilitySlots(DDDSample1DbContext context, AvailabilitySlot availabilitySlot)
  {
    if (!context.AvailabilitySlots.Any())
    {
      context.AvailabilitySlots.Add(availabilitySlot);
    }
  }


}
