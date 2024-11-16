import { Component, OnInit } from '@angular/core';
import { StaffService } from '../../services/staff.service';
import { ActivatedRoute } from '@angular/router';
import { CommonModule } from '@angular/common';
import { Staff } from '../../models/staff';

@Component({
  selector: 'app-staff-details',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './staff-details.component.html',
  styleUrl: './staff-details.component.css'
})
export class StaffDetailsComponent implements OnInit {
  staff: Staff | undefined;
  errorMessage = '';

  constructor(private staffService: StaffService, private route: ActivatedRoute) { }

  ngOnInit(): void {
    const id = this.route.snapshot.paramMap.get('id');
    if (id) {
      this.getStaffDetails(id);
    } else {
      this.errorMessage = 'Invalid staff';
    }
  }

  getStaffDetails(id: string): void {
    this.errorMessage = '';

    this.staffService.getStaffById(id).subscribe({
      next: (data: Staff) => this.staff = data,
      error: (err: any) => {
        console.log('Failed to fetch staff details', err);
        this.errorMessage = 'Failed to fetch staff details';
      }
    });
  };


}
