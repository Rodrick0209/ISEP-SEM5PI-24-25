using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.OperationRooms;
using System.Data;

namespace DDDSample1.Infrastructure.OperationRooms
{
    internal class OperationRoomEntityTypeConfiguration : IEntityTypeConfiguration<OperationRoom>
    {
        public void Configure(EntityTypeBuilder<OperationRoom> builder)
        {
            // Definindo a chave primária
            builder.HasKey(b => b.Id);

            // Configurando o RoomNumber como um objeto próprio
            builder.OwnsOne(b => b.RoomNumber, rn =>
            {
                rn.Property(r => r.roomNumber).IsRequired(); // Assume que RoomNumber tem uma propriedade Value
                rn.HasIndex(r => r.roomNumber).IsUnique(); // Define o RoomNumber como único
            });

            // Configurando a relação com RoomType como uma entidade de um agregado
            builder.HasOne(b => b.RoomType);
        
            // Configurando o RoomCapacity como um objeto próprio
            builder.OwnsOne(b => b.RoomCapacity, rc =>
            {
                rc.Property(r => r.roomCapacity).IsRequired(); // Assume que RoomCapacity tem uma propriedade Capacity
            });

            // Configurando o MaintenanceSlots como um objeto próprio
            builder.OwnsMany(b => b.MaintenanceSlots, ms =>
            {
                ms.WithOwner(); // Relaciona com OperationRoom
                ms.Property(m => m.Date).IsRequired(); // Propriedade Date em MaintenanceSlots

                ms.OwnsMany(m => m.TimeSlots, ts =>
                {
                    ts.WithOwner(); // Relaciona com MaintenanceSlots
                    ts.Property(t => t.StartMinute).IsRequired();
                    ts.Property(t => t.EndMinute).IsRequired();
                });
            });

            




        }
    }
}