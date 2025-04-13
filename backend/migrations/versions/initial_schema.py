
"""Initial database schema

Revision ID: 0001
Revises: 
Create Date: 2025-04-13

"""
from alembic import op
import sqlalchemy as sa

# revision identifiers
revision = '0001'
down_revision = None
branch_labels = None
depends_on = None

def upgrade():
    # Create all tables based on models
    op.create_table(
        'uploaded_files',
        sa.Column('id', sa.String(), nullable=False),
        sa.Column('filename', sa.String(), nullable=False),
        sa.Column('content', sa.Text(), nullable=False),
        sa.Column('size', sa.Integer(), nullable=False),
        sa.Column('lines', sa.Integer(), nullable=False),
        sa.Column('upload_date', sa.DateTime(), nullable=True),
        sa.PrimaryKeyConstraint('id')
    )
    
    op.create_table(
        'analysis_results',
        sa.Column('id', sa.String(), nullable=False),
        sa.Column('file_id', sa.String(), nullable=False),
        sa.Column('summary', sa.Text(), nullable=True),
        sa.Column('business_rules', sa.Text(), nullable=True),
        sa.Column('complexity_score', sa.Float(), nullable=True),
        sa.Column('lines_of_code', sa.Integer(), nullable=True),
        sa.Column('comment_percentage', sa.Float(), nullable=True),
        sa.Column('cyclomatic_complexity', sa.Float(), nullable=True),
        sa.Column('analysis_date', sa.DateTime(), nullable=True),
        sa.ForeignKeyConstraint(['file_id'], ['uploaded_files.id'], ondelete='CASCADE'),
        sa.PrimaryKeyConstraint('id')
    )
    
    op.create_table(
        'code_chunks',
        sa.Column('id', sa.String(), nullable=False),
        sa.Column('analysis_id', sa.String(), nullable=False),
        sa.Column('chunk_type', sa.String(), nullable=False),
        sa.Column('name', sa.String(), nullable=False),
        sa.Column('content', sa.Text(), nullable=False),
        sa.Column('start_line', sa.Integer(), nullable=False),
        sa.Column('end_line', sa.Integer(), nullable=False),
        sa.Column('line_count', sa.Integer(), nullable=False),
        sa.ForeignKeyConstraint(['analysis_id'], ['analysis_results.id'], ondelete='CASCADE'),
        sa.PrimaryKeyConstraint('id')
    )
    
    op.create_table(
        'feedback',
        sa.Column('id', sa.String(), nullable=False),
        sa.Column('file_id', sa.String(), nullable=False),
        sa.Column('chunk_id', sa.String(), nullable=True),
        sa.Column('rating', sa.Integer(), nullable=False),
        sa.Column('comment', sa.Text(), nullable=True),
        sa.Column('corrected_summary', sa.Text(), nullable=True),
        sa.Column('user_identifier', sa.String(), nullable=True),
        sa.Column('created_at', sa.DateTime(), nullable=True),
        sa.Column('is_processed_for_training', sa.Boolean(), nullable=True),
        sa.ForeignKeyConstraint(['chunk_id'], ['code_chunks.id'], ondelete='SET NULL'),
        sa.ForeignKeyConstraint(['file_id'], ['uploaded_files.id'], ondelete='CASCADE'),
        sa.PrimaryKeyConstraint('id')
    )
    
    op.create_table(
        'model_versions',
        sa.Column('id', sa.String(), nullable=False),
        sa.Column('model_name', sa.String(), nullable=False),
        sa.Column('version', sa.String(), nullable=False),
        sa.Column('created_at', sa.DateTime(), nullable=True),
        sa.Column('is_active', sa.Boolean(), nullable=True),
        sa.PrimaryKeyConstraint('id')
    )
    
    op.create_table(
        'training_jobs',
        sa.Column('id', sa.String(), nullable=False),
        sa.Column('feedback_count', sa.Integer(), nullable=False),
        sa.Column('status', sa.String(), nullable=False),
        sa.Column('started_at', sa.DateTime(), nullable=True),
        sa.Column('completed_at', sa.DateTime(), nullable=True),
        sa.Column('error_message', sa.Text(), nullable=True),
        sa.PrimaryKeyConstraint('id')
    )

def downgrade():
    # Drop all tables in reverse creation order
    op.drop_table('training_jobs')
    op.drop_table('model_versions')
    op.drop_table('feedback')
    op.drop_table('code_chunks')
    op.drop_table('analysis_results')
    op.drop_table('uploaded_files')
