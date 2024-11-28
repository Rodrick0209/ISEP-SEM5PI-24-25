
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
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.Appointments;
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
    Patient rodrick = new Patient(
      "Rodrick",
      "2022-10-01",
      "male",
      "1221083@isep.ipp.pt",
      "+351 123456788",
      "Main Street 123",
      "1234-567",
      "Los Angeles",
      "USA",
      "Jane Cena",
      "jane.cena@gmail.com",
      "+351 234567234",
      "202410000006");

    var patients = new List<Patient> { johnCena, johnCena2, johnCena3, johnCena4, johnCena5, rodrick };

    johnCena.MedicalHistory.ChangeMedicalConditions("Healthy");
    johnCena2.MedicalHistory.ChangeMedicalConditions("Ashma");
    johnCena3.MedicalHistory.ChangeMedicalConditions("Covid");

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


    // Create required staff
    var requiredStaff1 = new RequiredStaff(10, specialization1.Id);
    var requiredStaff2 = new RequiredStaff(20, specialization2.Id);
    var requiredStaffList1 = new List<RequiredStaff> { requiredStaff1, requiredStaff2 };
    var requiredStaffList2 = new List<RequiredStaff> { new RequiredStaff(20, specialization2.Id) }; // New instance
    var requiredStaffList3 = new List<RequiredStaff> { new RequiredStaff(2, specialization2.Id) }; // New instance

    PasswordHasher hasher = new PasswordHasher();
    string password = hasher.HashPassword("password");
    var user = new User("D202512345@gmail.com", "doctor", password);
    var user2 = new User("D202512344@gmail.com", "doctor", password);
    var user3 = new User("admin@teste.com", "admin", password);
    var user4 = new User("john.cena@gmail.com", "patient", password);
    var user5 = new User("rodrigocastro2004@gmail.com", "admin", password);

    var users = new List<User> { user, user2, user3, user4, user5 };

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
    operationRequest.staffAssignedSurgery.addStaffSurgeryPhase(new StaffId("D202512345"));
    operationRequest.staffAssignedSurgery.addStaffSurgeryPhase(new StaffId("D202512346"));
    operationRequest.staffAssignedSurgery.addStaffAnesthesyPhase(new StaffId("D202512344"));
    SeedOperationRequest(context, operationRequest);
    var staffAssignedSurgery = operationRequest.staffAssignedSurgery;
    SeedStaffAssignedSurgery(context, staffAssignedSurgery);

    var operationRequest2 = new OperationRequest("2025-02-19", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest2);

    var operationRequest3 = new OperationRequest("2025-02-20", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest3);

    var operationRequest4 = new OperationRequest("2025-02-23", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest4);

    var operationRequest5 = new OperationRequest("2025-02-24", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest5);
    var operationRequest6 = new OperationRequest("2025-02-25", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest6);
    var operationRequest7 = new OperationRequest("2025-02-26", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest7);
    var operationRequest8 = new OperationRequest("2025-02-26", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest8);

    var operationRequest9 = new OperationRequest("2025-02-26", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest9);

    var operationRequest10 = new OperationRequest("2025-02-26", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest10);

    var operationRequest11 = new OperationRequest("2025-02-26", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest11);

    var operationRequest12 = new OperationRequest("2025-02-26", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest12);

    var operationRequest13 = new OperationRequest("2025-02-26", "emergency", johnCena.Id.AsString(), operationType2.Id.AsString(), new StaffId("D202512345").AsString(), new StaffId("D202512345").AsString());
    SeedOperationRequest(context, operationRequest13);


    Staff staff = new Staff(new StaffId("D202512345"), "staff", "12345", specialization1.Id, "email@gmail.com", "+951999999999", "Doctor");
    SeedStaff(context, staff);



    var availableSlot2 = new AvailabilitySlot(staff.Id.AsString());
    DailyAvailability dailyAvailability = new DailyAvailability(new DateOnly(2025, 10, 28));
    dailyAvailability.AddTimeSlot(720, 840);
    dailyAvailability.AddTimeSlot(1080, 1200);
    dailyAvailability.AddTimeSlot(1200, 1300);
    SeedDailyAvailability(context, dailyAvailability);

    availableSlot2.Availability.Add(dailyAvailability);


    SeedAvailabilitySlots(context, availableSlot2);

    Staff staff2 = new Staff(new StaffId("D202512344"), "staffMario", "12346", specialization3.Id, "emaill@gmail.com", "+951999999998", "Doctor");
    SeedStaff(context, staff2);



    var operationRoom = new OperationRoom("or1", "boas", "10");
    operationRoom.AddMaintenance(new DateOnly(2025, 01, 01), 720, 840);
    operationRoom.AddMaintenance(new DateOnly(2025, 01, 01), 1080, 1200);
    operationRoom.AddMaintenance(new DateOnly(2025, 01, 01), 1200, 1300);
    SeedOperationRoom(context, operationRoom);

    OperationRoom teste = new OperationRoom("or8", "description8", "10");

    var rooms = new List<OperationRoom>
    {
      new OperationRoom("or2", "description2", "10"),
      new OperationRoom("or3", "description3", "10"),
      new OperationRoom("or4", "description4", "10"),
      new OperationRoom("or5", "description5", "10"),
      new OperationRoom("or6", "description6", "10"),
      new OperationRoom("or7", "description7", "10"),
      teste,
      new OperationRoom("or9", "description9", "10"),
      new OperationRoom("or10", "description10", "10")
    };

    context.OperationRooms.AddRange(rooms);

    context.SaveChanges();

    int currentHourInMinutes = DateTime.Now.Hour * 60 + DateTime.Now.Minute;
    int startMinute = currentHourInMinutes + 1;
    int endMinute = Math.Min(currentHourInMinutes + 120, 1440);
    var appointmentTimeSlot = new AppointmentTimeSlot(new DateOnly(DateTime.Now.Year, DateTime.Now.Month, DateTime.Now.Day), new TimeSlot(startMinute, endMinute));
    var appointment = new Appointment(appointmentTimeSlot, operationRoom.Id, operationRequest.Id);
    SeedAppointments(context, appointment);
    context.SaveChanges();
  }

  private static void SeedOperationRequest(DDDSample1DbContext context, OperationRequest operationRequest)
  {
    if (!context.OperationRequests.Any())
    {
      context.OperationRequests.Add(operationRequest);
    }
  }

  private static void SeedAvailabilitySlots(DDDSample1DbContext context, AvailabilitySlot availabilitySlot)
  {
    if (!context.AvailabilitySlots.Any())
    {
      context.AvailabilitySlots.Add(availabilitySlot);
    }
  }


  private static void SeedDailyAvailability(DDDSample1DbContext context, DailyAvailability dailyAvailability)
  {
    if (!context.DailyAvailabilities.Any())
    {
      context.DailyAvailabilities.Add(dailyAvailability);
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

  private static void SeedStaffAssignedSurgery(DDDSample1DbContext context, StaffAssignedSurgery staffAssignedSurgery)
  {
    if (!context.StaffAssignedSurgeries.Any())
    {
      context.StaffAssignedSurgeries.Add(staffAssignedSurgery);
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

  public static void SeedOperationRoom(DDDSample1DbContext context, OperationRoom operationRoom)
  {
    if (!context.OperationRooms.Any())
    {
      context.OperationRooms.Add(operationRoom);
    }
  }

  public static void SeedAppointments(DDDSample1DbContext context, Appointment appointment)
  {
    if (!context.Appointments.Any())
    {
      context.Appointments.Add(appointment);
    }
  }

  public static async Task UnseedAsync(IServiceProvider serviceProvider)
  {
    using var scope = serviceProvider.CreateScope();
    var context = scope.ServiceProvider.GetRequiredService<DDDSample1DbContext>();

    // Remove appointments
    var appointments = context.Appointments.ToList();
    context.Appointments.RemoveRange(appointments);

    // Remove availability slots
    var availabilitySlots = context.AvailabilitySlots.ToList();
    context.AvailabilitySlots.RemoveRange(availabilitySlots);

    // Remove operation rooms
    var operationRooms = context.OperationRooms.ToList();
    context.OperationRooms.RemoveRange(operationRooms);

    // Remove staff
    var staffMembers = context.StaffMembers.ToList();
    context.StaffMembers.RemoveRange(staffMembers);

    // Remove operation requests
    var operationRequests = context.OperationRequests.ToList();
    context.OperationRequests.RemoveRange(operationRequests);

    // Remove operation types
    var operationTypes = context.OperationTypes.ToList();
    context.OperationTypes.RemoveRange(operationTypes);

    // Remove phases
    var phases = context.Phases.ToList();
    context.Phases.RemoveRange(phases);

    // Remove specializations
    var specializations = context.Specializations.ToList();
    context.Specializations.RemoveRange(specializations);

    // Remove patients
    var patients = context.Patients.ToList();
    context.Patients.RemoveRange(patients);

    // Remove users
    var users = context.Users.ToList();
    context.Users.RemoveRange(users);

    // Save the changes to the database
    await context.SaveChangesAsync();
  }

}
