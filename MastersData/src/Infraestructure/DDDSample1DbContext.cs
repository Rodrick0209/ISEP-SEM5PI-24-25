using Microsoft.EntityFrameworkCore;
using DDDSample1.Domain.Categories;
using DDDSample1.Domain.Products;
using DDDSample1.Domain.Families;
using DDDSample1.Infrastructure.Categories;
using DDDSample1.Infrastructure.Products;
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
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Infrastructure.AvailabilitySlots;

namespace DDDSample1.Infrastructure
{
    public class DDDSample1DbContext : DbContext
    {
        /*public DbSet<Category> Categories { get; set; }*/

        public DbSet<Product> Products { get; set; }

        public DbSet<Family> Families { get; set; }

        public DbSet<User> Users { get; set; }

        public DbSet<OperationRequest> OperationRequests { get; set; }
        public DbSet<Staff> StaffMembers { get; set; }
        public DbSet<AvailabilitySlot> AvailabilitySlots { get; set; }



        public DbSet<OperationType> OperationTypes { get; set; }

        public DbSet<Phase> Phases { get; set; }

        public DbSet<Patient> Patients { get; set; }

        public DbSet<Specialization> Specializations { get; set; }

        public DbSet<OperationRequestLogger> OperationRequestLoggers { get; set; }

        public DbSet<PatientLogger> PatientLoggers { get; set; }
        public DbSet<StaffLogger> StaffLoggers { get; set; } 

        public DDDSample1DbContext(DbContextOptions options) : base(options)
        {

        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.ApplyConfiguration(new CategoryEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new ProductEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new FamilyEntityTypeConfiguration());
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
            modelBuilder.Entity<Phase>(entity =>
            {
                entity.OwnsMany(p => p.requiredStaff, rs =>
                {
                    rs.WithOwner().HasForeignKey("PhaseId");
                    rs.Property<int>("Id");
                    rs.HasKey("Id");
                });
            });
            
            base.OnModelCreating(modelBuilder);
            

           
        }
    }
}