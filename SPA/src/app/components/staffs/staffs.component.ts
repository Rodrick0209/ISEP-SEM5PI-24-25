import { Component } from '@angular/core';
import { ListStaffsComponent } from '../list-staffs/list-staffs.component';
import { CreateStaffComponent } from '../create-staff/create-staff.component';

@Component({
  selector: 'app-staffs',
  standalone: true,
  imports: [ListStaffsComponent, CreateStaffComponent],
  templateUrl: './staffs.component.html',
  styleUrl: './staffs.component.css'
})
export class StaffsComponent {
  staffs: string[] = [];

  onStaffAdded(newStaff: string) {
    this.staffs.push(newStaff); // Adiciona novo paciente à lista
  }
}
