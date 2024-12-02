using Microsoft.EntityFrameworkCore;

using DDDSample1.Domain.User;
using DDDSample1.Infrastructure.Users;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Infrastructure.OperationRequests;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Infrastructure.OperationTypes;
using DDDSample1.Domain.Patients;
using DDDSample1.Infrastructure.Patients;
using DDDSample1.Domain.Specializations;
using DDDSample1.Infrastructure.Specializations;
using DDDSample1.Domain.OperationRequestLoggers;
using DDDSample1.Infrastructure.OperationRequestLoggers;
using DDDSample1.Domain.PatientLoggers;
using DDDSample.Infrastructure.PatientLoggers;
using DDDSample1.Domain.StaffLoggers;
using DDDSample.Infrastructure.StaffLoggers;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Infrastructure.StaffMembers;
using DDDSample1.Domain.Appointments;
using DDDSample1.Infrastructure.Appointments;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Infrastructure.AvailabilitySlots;
using DDDSample1.Infrastructure.OperationRooms;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.RoomTypes;
using DDDSample1.Infrastructure.RoomTypes;

namespace DDDSample1.Infrastructure
{
    public class DDDSample1DbContext : DbContext
    {
        /*public DbSet<Category> Categories { get; set; }*/



        public DbSet<User> Users { get; set; }

        public DbSet<StaffAssignedSurgery> StaffAssignedSurgeries { get; set; }


        public DbSet<OperationRequest> OperationRequests { get; set; }
        public DbSet<Staff> StaffMembers { get; set; }
        public DbSet<AvailabilitySlot> AvailabilitySlots { get; set; }

        public DbSet<DailyAvailability> DailyAvailabilities { get; set; }
        public DbSet<OperationRoom> OperationRooms { get; set; }

        public DbSet<OperationType> OperationTypes { get; set; }

        public DbSet<Phase> Phases { get; set; }

        public DbSet<Patient> Patients { get; set; }

        public DbSet<MedicalHistory> MedicalHistories { get; set; }

        public DbSet<Specialization> Specializations { get; set; }

        public DbSet<OperationRequestLogger> OperationRequestLoggers { get; set; }

        public DbSet<OperationTypeLogger> OperationTypeLoggers { get; set; }

        public DbSet<PatientLogger> PatientLoggers { get; set; }

        public DbSet<StaffLogger> StaffLoggers { get; set; }
        public DbSet<Appointment> Appointments { get; set; }
        public DbSet<RoomType> RoomTypes { get; set; }

        public DDDSample1DbContext(DbContextOptions options) : base(options)
        {

        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.ApplyConfiguration(new UserEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new OperationRequestEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new OperationTypeEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new PatientEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new SpecializationEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new OperationRequestLoggerEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new PatientLoggerEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new StaffEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new StaffLoggerEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new AvailabilitySlotEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new PhaseEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new OperationRoomEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new AppointmentEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new RoomTypeEntityTypeConfiguration());


            modelBuilder.ApplyConfiguration(new StaffAssignedSurgeryEntityTypeConfiguration());
            modelBuilder.Entity<StaffAssignedSurgery>(entity =>
            {
                // Configuração de staffAnesthesyPhase
                entity.OwnsMany(p => p.staffAnesthesyPhase, rs =>
                {
                    rs.WithOwner().HasForeignKey("StaffAssignedSurgeryId");

                    // Mapeia o valor interno de StaffId (Guid)
                    rs.Property(x => x.Value)
                    .HasColumnName("StaffId"); // Nome da coluna no banco de dados
                    rs.HasKey(x => x.Value);    // Define o valor como chave
                });

                // Configuração de staffSurgeryPhase
                entity.OwnsMany(p => p.staffSurgeryPhase, rs =>
                {
                    rs.WithOwner().HasForeignKey("StaffAssignedSurgeryId");

                    // Mapeia o valor interno de StaffId (Guid)
                    rs.Property(x => x.Value)
                    .HasColumnName("StaffId");
                    rs.HasKey(x => x.Value);
                });
            });



            modelBuilder.Entity<Phase>(entity =>
                entity.OwnsMany(p => p.requiredStaff, rs =>
                {
                    rs.WithOwner().HasForeignKey("PhaseId");
                    rs.Property<int>("Id");
                    rs.HasKey("Id");
                })
                );


            modelBuilder.ApplyConfiguration(new DailyAvailabilityEntityTypeConfiguration());

            modelBuilder.Entity<DailyAvailability>(entity =>
            {
                entity.OwnsMany(p => p.TimeSlots, rs =>
                {
                    rs.WithOwner().HasForeignKey("DailyAvailabilityId");
                    rs.Property<int>("Id");
                    rs.HasKey("Id");
                });
            });


            base.OnModelCreating(modelBuilder);



        }
    }
}